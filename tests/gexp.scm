;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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
  #:use-module (guix grafts)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix tests)
  #:use-module ((guix build utils) #:select (with-directory-excursion))
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module ((guix diagnostics) #:select (guix-warning-port))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ftw))

;; Test the (guix gexp) module.

(define %store
  (open-connection-for-tests))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)

;; For white-box testing.
(define (gexp-inputs x)
  ((@@ (guix gexp) gexp-inputs) x))
(define (gexp-outputs x)
  ((@@ (guix gexp) gexp-outputs) x))
(define (gexp->sexp . x)
  (apply (@@ (guix gexp) gexp->sexp) x))

(define* (gexp->sexp* exp #:optional target)
  (run-with-store %store (gexp->sexp exp (%current-system) target)
                  #:guile-for-build (%guile-for-build)))

(define (gexp-input->tuple input)
  (list (gexp-input-thing input) (gexp-input-output input)
        (gexp-input-native? input)))

(define %extension-package
  ;; Example of a package to use when testing 'with-extensions'.
  (dummy-package "extension"
                 (build-system trivial-build-system)
                 (arguments
                  `(#:guile ,%bootstrap-guile
                    #:modules ((guix build utils))
                    #:builder
                    (begin
                      (use-modules (guix build utils))
                      (let* ((out (string-append (assoc-ref %outputs "out")
                                                 "/share/guile/site/"
                                                 (effective-version))))
                        (mkdir-p out)
                        (call-with-output-file (string-append out "/hg2g.scm")
                          (lambda (port)
                            (define defmod 'define-module) ;fool Geiser
                            (write `(,defmod (hg2g)
                                      #:export (the-answer))
                                   port)
                            (write '(define the-answer 42) port)))))))))


(test-begin "gexp")

(test-equal "no references"
  '(display "hello gexp->approximate-sexp!")
  (gexp->approximate-sexp #~(display "hello gexp->approximate-sexp!")))

(test-equal "unquoted gexp"
  '(display "hello")
  (let ((inside #~"hello"))
    (gexp->approximate-sexp #~(display #$inside))))

(test-equal "unquoted gexp (native)"
  '(display "hello")
  (let ((inside #~"hello"))
    (gexp->approximate-sexp #~(display #+inside))))

(test-equal "spliced gexp"
  '(display '(fresh vegetables))
  (let ((inside #~(fresh vegetables)))
    (gexp->approximate-sexp #~(display '(#$@inside)))))

(test-equal "unspliced gexp, approximated"
  ;; (*approximate*) is really an implementation detail
  '(display '(*approximate*))
  (let ((inside (file-append coreutils "/bin/hello")))
    (gexp->approximate-sexp #~(display '(#$@inside)))))

(test-equal "unquoted gexp, approximated"
  '(display '(*approximate*))
  (let ((inside (file-append coreutils "/bin/hello")))
    (gexp->approximate-sexp #~(display '#$inside))))

(test-equal "no refs"
  '(display "hello!")
  (let ((exp (gexp (display "hello!"))))
    (and (gexp? exp)
         (null? (gexp-inputs exp))
         (gexp->sexp* exp))))

(test-equal "sexp->gexp"
  '(a b (c d) e)
  (let ((exp (sexp->gexp '(a b (c d) e))))
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
           ((input)
            (eq? (gexp-input-thing input) coreutils)))
         (equal? `(display ,(derivation->output-path
                             (package-derivation %store coreutils)))
                 (gexp->sexp* exp)))))

(test-assert "one input package, dotted list"
  (let ((exp (gexp (coreutils . (ungexp coreutils)))))
    (and (gexp? exp)
         (match (gexp-inputs exp)
           ((input)
            (eq? (gexp-input-thing input) coreutils)))
         (equal? `(coreutils . ,(derivation->output-path
                                 (package-derivation %store coreutils)))
                 (gexp->sexp* exp)))))

(test-assert "one input origin"
  (let ((exp (gexp (display (ungexp (package-source coreutils))))))
    (and (gexp? exp)
         (match (gexp-inputs exp)
           ((input)
            (and (eq? (gexp-input-thing input) (package-source coreutils))
                 (string=? (gexp-input-output input) "out"))))
         (equal? `(display ,(derivation->output-path
                             (package-source-derivation
                              %store (package-source coreutils))))
                 (gexp->sexp* exp)))))

(test-assert "one local file"
  (let* ((file  (search-path %load-path "guix.scm"))
         (local (local-file file))
         (exp   (gexp (display (ungexp local))))
         (intd  (add-to-store %store (basename file) #f
                              "sha256" file)))
    (and (gexp? exp)
         (match (gexp-inputs exp)
           ((input)
            (and (eq? (gexp-input-thing input) local)
                 (string=? (gexp-input-output input) "out"))))
         (equal? `(display ,intd) (gexp->sexp* exp)))))

(test-assert "one local file, symlink"
  (let ((file (search-path %load-path "guix.scm"))
        (link (tmpnam)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (symlink (canonicalize-path file) link)
        (let* ((local (local-file link "my-file" #:recursive? #f))
               (exp   (gexp (display (ungexp local))))
               (intd  (add-to-store %store "my-file" #f
                                    "sha256" file)))
          (and (gexp? exp)
               (match (gexp-inputs exp)
                 ((input)
                  (and (eq? (gexp-input-thing input) local)
                       (string=? (gexp-input-output input) "out"))))
               (equal? `(display ,intd) (gexp->sexp* exp)))))
      (lambda ()
        (false-if-exception (delete-file link))))))

(test-equal "local-file, relative file name"
  (canonicalize-path (search-path %load-path "guix/base32.scm"))
  (let ((directory (dirname (search-path %load-path
                                         "guix/build-system/gnu.scm"))))
    (with-directory-excursion directory
        (let ((file (local-file "../guix/base32.scm")))
          (local-file-absolute-file-name file)))))

(test-equal "local-file, non-literal relative file name"
  (canonicalize-path (search-path %load-path "guix/base32.scm"))
  (let ((directory (dirname (search-path %load-path
                                         "guix/build-system/gnu.scm"))))
    (with-directory-excursion directory
      (let ((file (local-file (string-copy "../base32.scm"))))
        (local-file-absolute-file-name file)))))

(test-assertm "local-file, #:select?"
  (mlet* %store-monad ((select? -> (lambda (file stat)
                                     (member (basename file)
                                             '("guix.scm" "tests"
                                               "gexp.scm"))))
                       (file -> (local-file ".." "directory"
                                            #:recursive? #t
                                            #:select? select?))
                       (dir (lower-object file)))
    (return (and (store-path? dir)
                 (equal? (scandir dir)
                         '("." ".." "guix.scm" "tests"))
                 (equal? (scandir (string-append dir "/tests"))
                         '("." ".." "gexp.scm"))))))

(test-assert "one plain file"
  (let* ((file     (plain-file "hi" "Hello, world!"))
         (exp      (gexp (display (ungexp file))))
         (expected (add-text-to-store %store "hi" "Hello, world!")))
    (and (gexp? exp)
         (match (gexp-inputs exp)
           ((input)
            (and (eq? (gexp-input-thing input) file)
                 (string=? (gexp-input-output input) "out"))))
         (equal? `(display ,expected) (gexp->sexp* exp)))))

(test-assert "same input twice"
  (let ((exp (gexp (begin
                     (display (ungexp coreutils))
                     (display (ungexp coreutils))))))
    (and (gexp? exp)
         (match (gexp-inputs exp)
           ((input)
            (and (eq? (gexp-input-thing input) coreutils)
                 (string=? (gexp-input-output input) "out"))))
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
      (lambda (input)
        (eq? (gexp-input-thing input) thing)))

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

(test-assert "file-append"
  (let* ((drv (package-derivation %store %bootstrap-guile))
         (fa  (file-append %bootstrap-guile "/bin/guile"))
         (exp #~(here we go #$fa)))
    (and (match (gexp->sexp* exp)
           (('here 'we 'go (? string? result))
            (string=? result
                      (string-append (derivation->output-path drv)
                                     "/bin/guile"))))
         (match (gexp-inputs exp)
           ((input)
            (and (eq? (gexp-input-thing input) fa)
                 (string=? (gexp-input-output input) "out")))))))

(test-assert "file-append, output"
  (let* ((drv (package-derivation %store glibc))
         (fa  (file-append glibc "/lib" "/debug"))
         (exp #~(foo #$fa:debug)))
    (and (match (gexp->sexp* exp)
           (('foo (? string? result))
            (string=? result
                      (string-append (derivation->output-path drv "debug")
                                     "/lib/debug"))))
         (match (gexp-inputs exp)
           ((input)
            (and (eq? (gexp-input-thing input) fa)
                 (string=? (gexp-input-output input) "debug")))))))

(test-assert "file-append, nested"
  (let* ((drv   (package-derivation %store glibc))
         (dir   (file-append glibc "/bin"))
         (slash (file-append dir "/"))
         (file  (file-append slash "getent"))
         (exp   #~(foo #$file)))
    (and (match (gexp->sexp* exp)
           (('foo (? string? result))
            (string=? result
                      (string-append (derivation->output-path drv)
                                     "/bin/getent"))))
         (match (gexp-inputs exp)
           ((input)
            (eq? (gexp-input-thing input) file))))))

(test-assert "file-append, raw store item"
  (let* ((obj   (plain-file "example.txt" "Hello!"))
         (a     (file-append obj "/a"))
         (b     (file-append a "/b"))
         (c     (file-append b "/c"))
         (exp   #~(list #$c))
         (item  (run-with-store %store (lower-object obj)))
         (lexp  (run-with-store %store (lower-gexp exp))))
    (and (equal? (lowered-gexp-sexp lexp)
                 `(list ,(string-append item "/a/b/c")))
         (equal? (lowered-gexp-sources lexp)
                 (list item))
         (null? (lowered-gexp-inputs lexp)))))

(test-assertm "with-parameters for %current-system"
  (mlet* %store-monad ((system -> (match (%current-system)
                                    ("aarch64-linux" "x86_64-linux")
                                    (_               "aarch64-linux")))
                       (drv    (package->derivation coreutils system))
                       (obj -> (with-parameters ((%current-system system))
                                 coreutils))
                       (result (lower-object obj)))
    (return (string=? (derivation-file-name drv)
                      (derivation-file-name result)))))

(test-assertm "with-parameters for %current-target-system"
  (mlet* %store-monad ((target -> "riscv64-linux-gnu")
                       (drv    (package->cross-derivation coreutils target))
                       (obj -> (with-parameters
                                   ((%current-target-system target))
                                 coreutils))
                       (result (lower-object obj)))
    (return (string=? (derivation-file-name drv)
                      (derivation-file-name result)))))

(test-assert "with-parameters + file-append"
  (let* ((system (match (%current-system)
                   ("aarch64-linux" "x86_64-linux")
                   (_               "aarch64-linux")))
         (drv    (package-derivation %store coreutils system))
         (param  (make-parameter 7))
         (exp    #~(here we go #$(with-parameters ((%current-system system)
                                                   (param 42))
                                   (if (= (param) 42)
                                       (file-append coreutils "/bin/touch")
                                       %bootstrap-guile)))))
    (match (gexp->sexp* exp)
      (('here 'we 'go (? string? result))
       (string=? result
                 (string-append (derivation->output-path drv)
                                "/bin/touch"))))))
(test-equal "let-system"
  (list `(begin ,(%current-system) #t) '(system-binding)
        'low '() '())
  (let* ((exp #~(begin
                  #$(let-system system system)
                  #t))
         (low (run-with-store %store (lower-gexp exp))))
    (list (lowered-gexp-sexp low)
          (match (gexp-inputs exp)
            ((input)
             (and (eq? (struct-vtable (gexp-input-thing input))
                       (@@ (guix gexp) <system-binding>))
                  (string=? (gexp-input-output input) "out")
                  '(system-binding)))
            (x x))
          'low
          (lowered-gexp-inputs low)
          (lowered-gexp-sources low))))

(test-equal "let-system, target"
  (list `(list ,(%current-system) #f)
        `(list ,(%current-system) "aarch64-linux-gnu"))
  (let ((exp #~(list #$@(let-system (system target)
                          (list system target)))))
    (list (gexp->sexp* exp)
          (gexp->sexp* exp "aarch64-linux-gnu"))))

(test-equal "let-system, ungexp-native, target"
  `(here it is: ,(%current-system) #f)
  (let ((exp #~(here it is: #+@(let-system (system target)
                                 (list system target)))))
    (gexp->sexp* exp "aarch64-linux-gnu")))

(test-equal "let-system, nested"
  (list `(system* ,(string-append "qemu-system-" (%current-system))
                  "-m" "256")
        '(system-binding))
  (let ((exp #~(system*
                #+(let-system (system target)
                    (file-append (@@ (gnu packages virtualization)
                                     qemu)
                                 "/bin/qemu-system-"
                                 system))
                "-m" "256")))
    (list (match (gexp->sexp* exp)
            (('system* command rest ...)
             `(system* ,(and (string-prefix? (%store-prefix) command)
                             (basename command))
                       ,@rest))
            (x x))
          (match (gexp-inputs exp)
            ((input)
             (and (eq? (struct-vtable (gexp-input-thing input))
                       (@@ (guix gexp) <system-binding>))
                  (string=? (gexp-input-output input) "out")
                  (gexp-input-native? input)
                  '(system-binding)))
            (x x)))))

(test-assert "let-system in file-append"
  (let ((mixed (file-append (let-system (system target)
                              (if (not target) grep sed))
                            "/bin"))
        (grep  (file-append grep "/bin"))
        (sed   (file-append sed "/bin")))
    (and (equal? (gexp->sexp* #~(list #$mixed))
                 (gexp->sexp* #~(list #$grep)))
         (equal? (gexp->sexp* #~(list #$mixed) "powerpc64le-linux-gnu")
                 (gexp->sexp* #~(list #$sed) "powerpc64le-linux-gnu")))))

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
                `((,%bootstrap-guile "out" #t)
                  (,coreutils "out" #f)
                  (,glibc "out" #t)
                  (,binutils "out" #f))
                (map gexp-input->tuple (gexp-inputs exp)))
         (equal? `(list ,guile ,cu ,libc ,bu)
                 (gexp->sexp* exp target)))))

(test-equal "ungexp + ungexp-native, nested"
  `((,%bootstrap-guile "out" #f) (,coreutils "out" #t))
  (let* ((exp (gexp (list (ungexp-native (gexp (ungexp coreutils)))
                          (ungexp %bootstrap-guile)))))
    (map gexp-input->tuple (gexp-inputs exp))))

(test-equal "ungexp + ungexp-native, nested, special mixture"
  `((,coreutils "out" #t))

  (let* ((foo (gexp (foo (ungexp-native coreutils))))
         (exp (gexp (bar (ungexp foo)))))
    (map gexp-input->tuple (gexp-inputs exp))))

(test-assert "input list"
  (let ((exp   (gexp (display
                      '(ungexp (list %bootstrap-guile coreutils)))))
        (guile (derivation->output-path
                (package-derivation %store %bootstrap-guile)))
        (cu    (derivation->output-path
                (package-derivation %store coreutils))))
    (and (lset= equal?
                `((,%bootstrap-guile "out" #f) (,coreutils "out" #f))
                (map gexp-input->tuple (gexp-inputs exp)))
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
                `((,%bootstrap-guile "out" #t) (,coreutils "out" #t)
                  (,glibc "out" #f) (,binutils "out" #f))
                (map gexp-input->tuple (gexp-inputs exp)))
         (equal? `(display (cons '(,guile ,cu) '(,xlibc ,xbu)))
                 (gexp->sexp* exp target)))))

(test-assert "input list splicing"
  (let* ((inputs  (list (gexp-input glibc "debug") %bootstrap-guile))
         (outputs (list (derivation->output-path
                         (package-derivation %store glibc)
                         "debug")
                        (derivation->output-path
                         (package-derivation %store %bootstrap-guile))))
         (exp     (gexp (list (ungexp-splicing (cons (+ 2 3) inputs))))))
    (and (lset= equal?
                `((,glibc "debug" #f) (,%bootstrap-guile "out" #f))
                (map gexp-input->tuple (gexp-inputs exp)))
         (equal? (gexp->sexp* exp)
                 `(list ,@(cons 5 outputs))))))

(test-assert "input list splicing + ungexp-native-splicing"
  (let* ((inputs (list (gexp-input glibc "debug" #:native? #t)
                       %bootstrap-guile))
         (exp    (gexp (list (ungexp-native-splicing (cons (+ 2 3) inputs))))))
    (and (lset= equal?
                `((,glibc "debug" #t) (,%bootstrap-guile "out" #t))
                (map gexp-input->tuple (gexp-inputs exp)))
         (equal? (gexp->sexp* exp)                ;native
                 (gexp->sexp* exp "mips64el-linux")))))

(test-assert "gexp list splicing + ungexp-splicing"
  (let* ((inner (gexp (ungexp-native glibc)))
         (exp   (gexp (list (ungexp-splicing (list inner))))))
    (and (equal? `((,glibc "out" #t))
                 (map gexp-input->tuple (gexp-inputs exp)))
         (equal? (gexp->sexp* exp)                ;native
                 (gexp->sexp* exp "mips64el-linux")))))

(test-equal "output list"
  2
  (let ((exp (gexp (begin (mkdir (ungexp output))
                          (mkdir (ungexp output "bar"))))))
    (length (gexp-outputs exp))))                ;XXX: <output-ref> is private

(test-assert "output list, combined gexps"
  (let* ((exp0  (gexp (mkdir (ungexp output))))
         (exp1  (gexp (mkdir (ungexp output "foo"))))
         (exp2  (gexp (begin (display "hi!") (ungexp exp0) (ungexp exp1)))))
    (and (lset= equal?
                (append (gexp-outputs exp0) (gexp-outputs exp1))
                (gexp-outputs exp2))
         (= 2 (length (gexp-outputs exp2))))))

(test-equal "output list, combined gexps, duplicate output"
  1
  (let* ((exp0 (gexp (mkdir (ungexp output))))
         (exp1 (gexp (begin (mkdir (ungexp output)) (ungexp exp0))))
         (exp2 (gexp (begin (mkdir (ungexp output)) (ungexp exp1)))))
    (length (gexp-outputs exp2))))

(test-assert "output list + ungexp-splicing list, combined gexps"
  (let* ((exp0  (gexp (mkdir (ungexp output))))
         (exp1  (gexp (mkdir (ungexp output "foo"))))
         (exp2  (gexp (begin (display "hi!")
                             (ungexp-splicing (list exp0 exp1))))))
    (and (lset= equal?
                (append (gexp-outputs exp0) (gexp-outputs exp1))
                (gexp-outputs exp2))
         (= 2 (length (gexp-outputs exp2))))))

(test-assertm "gexp->file"
  (mlet* %store-monad ((exp -> (gexp (display (ungexp %bootstrap-guile))))
                       (guile  (package-file %bootstrap-guile))
                       (sexp   (gexp->sexp exp (%current-system) #f))
                       (drv    (gexp->file "foo" exp))
                       (out -> (derivation->output-path drv))
                       (done   (built-derivations (list drv)))
                       (refs   (references* out)))
    (return (and (equal? sexp (call-with-input-file out read))
                 (equal? (list guile) refs)))))

(test-assertm "gexp->file + file-append"
  (mlet* %store-monad ((exp -> #~#$(file-append %bootstrap-guile
                                                "/bin/guile"))
                       (guile  (package-file %bootstrap-guile))
                       (drv    (gexp->file "foo" exp))
                       (out -> (derivation->output-path drv))
                       (done   (built-derivations (list drv)))
                       (refs   (references* out)))
    (return (and (equal? (string-append guile "/bin/guile")
                         (call-with-input-file out read))
                 (equal? (list guile) refs)))))

(test-assertm "gexp->file + #:splice?"
  (mlet* %store-monad ((exp -> (list
                                #~(define foo 'bar)
                                #~(define guile #$%bootstrap-guile)))
                       (guile  (package-file %bootstrap-guile))
                       (drv    (gexp->file "splice" exp #:splice? #t))
                       (out -> (derivation->output-path drv))
                       (done   (built-derivations (list drv)))
                       (refs   (references* out)))
    (pk 'splice out)
    (return (and (equal? `((define foo 'bar)
                           (define guile ,guile)
                           ,(call-with-input-string "" read))
                         (call-with-input-file out
                           (lambda (port)
                             (list (read port) (read port) (read port)))))
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
                       (refs    (references* out))
                       (refs2   (references* out2))
                       (guile   (package-file %bootstrap-guile "bin/guile")))
    (return (and (string=? (readlink (string-append out "/foo")) guile)
                 (string=? (readlink out2) file)
                 (equal? refs (list (dirname (dirname guile))))
                 (equal? refs2 (list file))
                 (null? (derivation-properties drv))))))

(test-assertm "gexp->derivation properties"
  (mlet %store-monad ((drv (gexp->derivation "foo"
                                             #~(mkdir #$output)
                                             #:properties '((type . test)))))
    (return (equal? '((type . test))
                    (derivation-properties drv)))))

(test-assertm "gexp->derivation vs. grafts"
  (mlet* %store-monad ((graft?  (set-grafting #f))
                       (p0 ->   (dummy-package "dummy"
                                               (arguments
                                                '(#:implicit-inputs? #f))))
                       (r  ->   (package (inherit p0) (name "DuMMY")))
                       (p1 ->   (package (inherit p0) (replacement r)))
                       (exp0 -> (gexp (frob (ungexp p0) (ungexp output))))
                       (exp1 -> (gexp (frob (ungexp p1) (ungexp output))))
                       (void    (set-guile-for-build %bootstrap-guile))
                       (drv0    (gexp->derivation "t" exp0 #:graft? #t))
                       (drv1    (gexp->derivation "t" exp1 #:graft? #t))
                       (drv1*   (gexp->derivation "t" exp1 #:graft? #f))
                       (_       (set-grafting graft?)))
    (return (and (not (string=? (derivation->output-path drv0)
                                (derivation->output-path drv1)))
                 (string=? (derivation->output-path drv0)
                           (derivation->output-path drv1*))))))

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

(test-assertm "gexp->derivation, local-file"
  (mlet* %store-monad ((file ->  (search-path %load-path "guix.scm"))
                       (intd     (interned-file file #:recursive? #f))
                       (local -> (local-file file))
                       (exp ->   (gexp (begin
                                         (stat (ungexp local))
                                         (symlink (ungexp local)
                                                  (ungexp output)))))
                       (drv      (gexp->derivation "local-file" exp)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (string=? (readlink (derivation->output-path drv))
                        intd)))))

(test-assertm "gexp->derivation, cross-compilation"
  (mlet* %store-monad ((target -> "mips64el-linux")
                       (exp    -> (gexp (list (ungexp coreutils)
                                              (ungexp output))))
                       (xdrv      (gexp->derivation "foo" exp
                                                    #:target target))
                       (refs      (references*
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
                       (refs      (references*
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

(test-assertm "gexp->derivation, store copy"
  (let ((build-one #~(call-with-output-file #$output
                       (lambda (port)
                         (display "This is the one." port))))
        (build-two (lambda (one)
                     #~(begin
                         (mkdir #$output)
                         (symlink #$one (string-append #$output "/one"))
                         (call-with-output-file (string-append #$output "/two")
                           (lambda (port)
                             (display "This is the second one." port))))))
        (build-drv #~(begin
                       (use-modules (guix build store-copy)
                                    (guix build utils)
                                    (srfi srfi-1))

                       (define (canonical-file? file)
                         ;; Copied from (guix tests).
                         (let ((st (lstat file)))
                           (or (not (string-prefix? (%store-directory) file))
                               (eq? 'symlink (stat:type st))
                               (and (= 1 (stat:mtime st))
                                    (zero? (logand #o222 (stat:mode st)))))))

                       (mkdir #$output)
                       (populate-store '("graph") #$output
                                       #:deduplicate? #f)

                       ;; Check whether 'populate-store' canonicalizes
                       ;; permissions and timestamps.
                       (unless (every canonical-file? (find-files #$output))
                         (error "not canonical!" #$output)))))
    (mlet* %store-monad ((one (gexp->derivation "one" build-one))
                         (two (gexp->derivation "two" (build-two one)))
                         (drv (gexp->derivation "store-copy" build-drv
                                                #:references-graphs
                                                `(("graph" ,two))
                                                #:modules
                                                '((guix build store-copy)
                                                  (guix progress)
                                                  (guix records)
                                                  (guix sets)
                                                  (guix build utils))))
                         (ok? (built-derivations (list drv)))
                         (out -> (derivation->output-path drv)))
      (let ((one (derivation->output-path one))
            (two (derivation->output-path two)))
        (return (and ok?
                     (file-exists? (string-append out "/" one))
                     (file-exists? (string-append out "/" two))
                     (file-exists? (string-append out "/" two "/two"))
                     (string=? (readlink (string-append out "/" two "/one"))
                               one)))))))

(test-assertm "imported-files"
  (mlet* %store-monad
      ((files -> `(("x"     . ,(search-path %load-path "ice-9/q.scm"))
                   ("a/b/c" . ,(search-path %load-path
                                            "guix/derivations.scm"))
                   ("p/q"   . ,(search-path %load-path "guix.scm"))
                   ("p/z"   . ,(search-path %load-path "guix/store.scm"))))
       (dir (imported-files files)))
    (mbegin %store-monad
      (return
       (every (match-lambda
                ((path . source)
                 (equal? (call-with-input-file (string-append dir "/" path)
                           get-bytevector-all)
                         (call-with-input-file source
                           get-bytevector-all))))
              files)))))

(test-assertm "imported-files with file-like objects"
  (mlet* %store-monad ((plain -> (plain-file "foo" "bar!"))
                       (q-scm -> (search-path %load-path "ice-9/q.scm"))
                       (files -> `(("a/b/c" . ,q-scm)
                                   ("p/q"   . ,plain)))
                       (drv      (imported-files files)))
    (mbegin %store-monad
      (built-derivations (list (pk 'drv drv)))
      (mlet %store-monad ((dir -> (derivation->output-path drv))
                          (plain* (text-file "foo" "bar!"))
                          (q-scm* (interned-file q-scm "c")))
        (return
         (and (file=? (string-append dir "/a/b/c") q-scm* stat)
              (file=? (string-append dir "/p/q") plain* stat)))))))

(test-equal "gexp-modules & ungexp"
  '((bar) (foo))
  ((@@ (guix gexp) gexp-modules)
   #~(foo #$(with-imported-modules '((foo)) #~+)
          #+(with-imported-modules '((bar)) #~-))))

(test-equal "gexp-modules & ungexp-splicing"
  '((foo) (bar))
  ((@@ (guix gexp) gexp-modules)
   #~(foo #$@(list (with-imported-modules '((foo)) #~+)
                   (with-imported-modules '((bar)) #~-)))))

(test-assert "gexp-modules deletes duplicates"   ;<https://bugs.gnu.org/32966>
  (let ((make-file (lambda ()
                     ;; Use 'eval' to make sure we get an object that's not
                     ;; 'eq?' nor 'equal?' due to the closures it embeds.
                     (eval '(scheme-file "bar.scm" #~(define-module (bar)))
                           (current-module)))))
    (define result
      ((@@ (guix gexp) gexp-modules)
       (with-imported-modules `(((bar) => ,(make-file))
                                ((bar) => ,(make-file))
                                (foo) (foo))
         #~+)))

    (match result
      (((('bar) '=> (? scheme-file?)) ('foo)) #t))))

(test-equal "gexp-modules and literal Scheme object"
  '()
  (gexp-modules #t))

(test-assert "gexp-modules, warning"
  (string-match "tests/gexp.scm:[0-9]+:[0-9]+: warning: \
importing.* \\(guix config\\) from the host"
                (call-with-output-string
                  (lambda (port)
                    (parameterize ((guix-warning-port port))
                      (let* ((x (with-imported-modules '((guix config))
                                  #~(+ 1 2 3)))
                             (y #~(+ 39 #$x)))
                        (gexp-modules y)))))))

(test-assertm "gexp->derivation #:modules"
  (mlet* %store-monad
      ((build ->  #~(begin
                      (use-modules (guix build utils))
                      (mkdir-p (string-append #$output "/guile/guix/nix"))
                      #t))
       (drv       (gexp->derivation "test-with-modules" build
                                    #:modules '((guix build utils)))))
    (mbegin %store-monad
      (built-derivations (list drv))
      (let* ((p (derivation->output-path drv))
             (s (stat (string-append p "/guile/guix/nix"))))
        (return (eq? (stat:type s) 'directory))))))

(test-assertm "gexp->derivation & with-imported-modules"
  ;; Same test as above, but using 'with-imported-modules'.
  (mlet* %store-monad
      ((build ->  (with-imported-modules '((guix build utils))
                    #~(begin
                        (use-modules (guix build utils))
                        (mkdir-p (string-append #$output "/guile/guix/nix"))
                        #t)))
       (drv       (gexp->derivation "test-with-modules" build)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (let* ((p (derivation->output-path drv))
             (s (stat (string-append p "/guile/guix/nix"))))
        (return (eq? (stat:type s) 'directory))))))

(test-assertm "gexp->derivation & nested with-imported-modules"
  (mlet* %store-monad
      ((build1 ->  (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils))
                         (mkdir-p (string-append #$output "/guile/guix/nix"))
                         #t)))
       (build2 ->  (with-imported-modules '((guix build bournish))
                     #~(begin
                         (use-modules (guix build bournish)
                                      (system base compile))
                         #+build1
                         (call-with-output-file (string-append #$output "/b")
                           (lambda (port)
                             (write
                              (read-and-compile (open-input-string "cd /foo")
                                                #:from %bournish-language
                                                #:to 'scheme)
                              port))))))
       (drv        (gexp->derivation "test-with-modules" build2)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (let* ((p (derivation->output-path drv))
             (s (stat (string-append p "/guile/guix/nix")))
             (b (string-append p "/b")))
        (return (and (eq? (stat:type s) 'directory)
                     (equal? '(chdir "/foo")
                             (call-with-input-file b read))))))))

(test-assertm "gexp->derivation & with-imported-module & computed module"
  (mlet* %store-monad
      ((module -> (scheme-file "x" #~(;; splice!
                                       (define-module (foo bar)
                                         #:export (the-answer))

                                       (define the-answer 42))
                               #:splice? #t))
       (build -> (with-imported-modules `(((foo bar) => ,module)
                                          (guix build utils))
                   #~(begin
                       (use-modules (guix build utils)
                                    (foo bar))
                       mkdir-p
                       (call-with-output-file #$output
                         (lambda (port)
                           (write the-answer port))))))
       (drv      (gexp->derivation "thing" build))
       (out ->   (derivation->output-path drv)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (= 42 (call-with-input-file out read))))))

(test-equal "gexp-extensions & ungexp"
  (list sed grep)
  ((@@ (guix gexp) gexp-extensions)
   #~(foo #$(with-extensions (list grep) #~+)
          #+(with-extensions (list sed)  #~-))))

(test-equal "gexp-extensions & ungexp-splicing"
  (list grep sed)
  ((@@ (guix gexp) gexp-extensions)
   #~(foo #$@(list (with-extensions (list grep) #~+)
                   (with-imported-modules '((foo))
                     (with-extensions (list sed) #~-))))))

(test-equal "gexp-extensions and literal Scheme object"
  '()
  ((@@ (guix gexp) gexp-extensions) #t))

(test-assertm "gexp->derivation & with-extensions"
  ;; Create a fake Guile extension and make sure it is accessible both to the
  ;; imported modules and to the derivation build script.
  (mlet* %store-monad
      ((extension -> %extension-package)
       (module -> (scheme-file "x" #~( ;; splice!
                                      (define-module (foo)
                                        #:use-module (hg2g)
                                        #:export (multiply))

                                      (define (multiply x)
                                        (* the-answer x)))
                               #:splice? #t))
       (build -> (with-extensions (list extension)
                   (with-imported-modules `((guix build utils)
                                            ((foo) => ,module))
                     #~(begin
                         (use-modules (guix build utils)
                                      (hg2g) (foo))
                         (call-with-output-file #$output
                           (lambda (port)
                             (write (list the-answer (multiply 2))
                                    port)))))))
       (drv      (gexp->derivation "thingie" build
                                   ;; %BOOTSTRAP-GUILE is 2.0.
                                   #:effective-version "2.0"))
       (out ->   (derivation->output-path drv)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (return (equal? '(42 84) (call-with-input-file out read))))))

(test-assertm "lower-gexp"
  (mlet* %store-monad
      ((extension -> %extension-package)
       (extension-drv (package->derivation %extension-package))
       (coreutils-drv (package->derivation coreutils))
       (exp ->   (with-extensions (list extension)
                   (with-imported-modules `((guix build utils))
                     #~(begin
                         (use-modules (guix build utils)
                                      (hg2g))
                         #$coreutils:debug
                         mkdir-p
                         the-answer))))
       (lexp     (lower-gexp exp
                             #:effective-version "2.0")))
    (define (matching-input drv output)
      (lambda (input)
        (and (eq? (derivation-input-derivation input) drv)
             (equal? (derivation-input-sub-derivations input)
                     (list output)))))

    (mbegin %store-monad
      (return (and (find (matching-input extension-drv "out")
                         (lowered-gexp-inputs (pk 'lexp lexp)))
                   (find (matching-input coreutils-drv "debug")
                         (lowered-gexp-inputs lexp))
                   (member (string-append
                            (derivation->output-path extension-drv)
                            "/share/guile/site/2.0")
                           (lowered-gexp-load-path lexp))
                   (= 2 (length (lowered-gexp-load-path lexp)))
                   (member (string-append
                            (derivation->output-path extension-drv)
                            "/lib/guile/2.0/site-ccache")
                           (lowered-gexp-load-compiled-path lexp))
                   (= 2 (length (lowered-gexp-load-compiled-path lexp)))
                   (eq? (derivation-input-derivation (lowered-gexp-guile lexp))
                        (%guile-for-build)))))))

(test-assertm "lower-gexp, raw-derivation-file"
  (mlet* %store-monad ((thing -> (program-file "prog" #~(display "hi!")))
                       (exp -> #~(list #$(raw-derivation-file thing)))
                       (drv  (lower-object thing))
                       (lexp (lower-gexp exp #:effective-version "2.0")))
    (return (and (equal? `(list ,(derivation-file-name drv))
                         (lowered-gexp-sexp lexp))
                 (equal? (list (derivation-file-name drv))
                         (lowered-gexp-sources lexp))
                 (null? (lowered-gexp-inputs lexp))))))

(test-eq "lower-gexp, non-self-quoting input"
  +
  (guard (c ((gexp-input-error? c)
             (gexp-error-invalid-input c)))
    (run-with-store %store
      (lower-gexp #~(foo #$+)))))

(test-equal "lower-gexp, character literal"
  '(#\+)
  (lowered-gexp-sexp
   (run-with-store %store
     (lower-gexp #~(#\+)))))

(test-assertm "gexp->derivation #:references-graphs"
  (mlet* %store-monad
      ((one (text-file "one" (random-text)))
       (two (gexp->derivation "two"
                              #~(symlink #$one #$output:chbouib)))
       (build -> (with-imported-modules '((guix build store-copy)
                                          (guix progress)
                                          (guix records)
                                          (guix sets)
                                          (guix build utils))
                   #~(begin
                       (use-modules (guix build store-copy))
                       (with-output-to-file #$output
                         (lambda ()
                           (write (map store-info-item
                                       (call-with-input-file "guile"
                                         read-reference-graph)))))
                       (with-output-to-file #$output:one
                         (lambda ()
                           (write (map store-info-item
                                       (call-with-input-file "one"
                                         read-reference-graph)))))
                       (with-output-to-file #$output:two
                         (lambda ()
                           (write (map store-info-item
                                       (call-with-input-file "two"
                                         read-reference-graph))))))))
       (drv (gexp->derivation "ref-graphs" build
                              #:references-graphs `(("one" ,one)
                                                    ("two" ,two "chbouib")
                                                    ("guile" ,%bootstrap-guile))))
       (ok? (built-derivations (list drv)))
       (guile-drv  (package->derivation %bootstrap-guile))
       (bash       (interned-file (search-bootstrap-binary "bash"
                                                           (%current-system))
                                  "bash" #:recursive? #t))
       (g-one   -> (derivation->output-path drv "one"))
       (g-two   -> (derivation->output-path drv "two"))
       (g-guile -> (derivation->output-path drv)))
    (return (and ok?
                 (equal? (call-with-input-file g-one read) (list one))
                 (lset= string=?
                        (call-with-input-file g-two read)
                        (list one (derivation->output-path two "chbouib")))

                 ;; Note: %BOOTSTRAP-GUILE depends on the bootstrap Bash.
                 (lset= string=?
                        (call-with-input-file g-guile read)
                        (list (derivation->output-path guile-drv) bash))))))

(test-assertm "gexp->derivation #:references-graphs cross-compilation"
  ;; The objects passed in #:references-graphs implicitly refer to
  ;; cross-compiled derivations.  Make sure this is the case.
  (mlet* %store-monad ((drv1 (lower-object coreutils (%current-system)
                                           #:target "i586-pc-gnu"))
                       (drv2 (lower-object coreutils (%current-system)
                                           #:target #f))
                       (drv3 (gexp->derivation "three"
                                               #~(symlink #$coreutils #$output)
                                               #:target "i586-pc-gnu"
                                               #:references-graphs
                                               `(("coreutils" ,coreutils))))
                       (refs (references* (derivation-file-name drv3))))
    (return (and (member (derivation-file-name drv1) refs)
                 (not (member (derivation-file-name drv2) refs))))))

(test-assertm "gexp->derivation #:allowed-references"
  (mlet %store-monad ((drv (gexp->derivation "allowed-refs"
                                             #~(begin
                                                 (mkdir #$output)
                                                 (chdir #$output)
                                                 (symlink #$output "self")
                                                 (symlink #$%bootstrap-guile
                                                          "guile"))
                                             #:allowed-references
                                             (list "out" %bootstrap-guile))))
    (built-derivations (list drv))))

(test-assertm "gexp->derivation #:allowed-references, specific output"
  (mlet* %store-monad ((in  (gexp->derivation "thing"
                                              #~(begin
                                                  (mkdir #$output:ok)
                                                  (mkdir #$output:not-ok))))
                       (drv (gexp->derivation "allowed-refs"
                                              #~(begin
                                                  (pk #$in:not-ok)
                                                  (mkdir #$output)
                                                  (chdir #$output)
                                                  (symlink #$output "self")
                                                  (symlink #$in:ok "ok"))
                                              #:allowed-references
                                              (list "out"
                                                    (gexp-input in "ok")))))
    (built-derivations (list drv))))

(test-assert "gexp->derivation #:allowed-references, disallowed"
  (let ((drv (run-with-store %store
               (gexp->derivation "allowed-refs"
                                 #~(begin
                                     (mkdir #$output)
                                     (chdir #$output)
                                     (symlink #$%bootstrap-guile "guile"))
                                 #:allowed-references '()))))
    (guard (c ((store-protocol-error? c) #t))
      (build-derivations %store (list drv))
      #f)))

(test-assertm "gexp->derivation #:disallowed-references, allowed"
  (mlet %store-monad ((drv (gexp->derivation "disallowed-refs"
                                             #~(begin
                                                 (mkdir #$output)
                                                 (chdir #$output)
                                                 (symlink #$output "self")
                                                 (symlink #$%bootstrap-guile
                                                          "guile"))
                                             #:disallowed-references '())))
    (built-derivations (list drv))))


(test-assert "gexp->derivation #:disallowed-references"
  (let ((drv (run-with-store %store
               (gexp->derivation "disallowed-refs"
                                 #~(begin
                                     (mkdir #$output)
                                     (chdir #$output)
                                     (symlink #$%bootstrap-guile "guile"))
                                 #:disallowed-references (list %bootstrap-guile)))))
    (guard (c ((store-protocol-error? c) #t))
      (build-derivations %store (list drv))
      #f)))

(define shebang
  (string-append "#!" (derivation->output-path (%guile-for-build))
                 "/bin/guile --no-auto-compile"))

;; If we're going to hit the silly shebang limit (128 chars on Linux-based
;; systems), then skip the following test.
(test-skip (if (> (string-length shebang) 127) 2 0))

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

(test-assert "gexp->script #:module-path"
  (call-with-temporary-directory
   (lambda (directory)
     (define str
       "Fake (guix base32) module!")

     (mkdir (string-append directory "/guix"))
     (call-with-output-file (string-append directory "/guix/base32.scm")
       (lambda (port)
         (write `(begin (define-module (guix base32))
                        (define-public %fake! ,str))
                port)))

     (run-with-store %store
       (mlet* %store-monad ((exp -> (with-imported-modules '((guix base32))
                                      (gexp (begin
                                              (use-modules (guix base32))
                                              (write (list %load-path
                                                           %fake!))))))
                            (drv    (gexp->script "guile-thing" exp
                                                  #:guile %bootstrap-guile
                                                  #:module-path (list directory)))
                            (out -> (derivation->output-path drv))
                            (done   (built-derivations (list drv))))
         (let* ((pipe  (open-input-pipe out))
                (data  (read pipe)))
           (return (and (zero? (close-pipe pipe))
                        (match data
                          ((load-path str*)
                           (and (string=? str* str)
                                (not (member directory load-path)))))))))))))

(test-assertm "program-file"
  (let* ((n      (random (expt 2 50)))
         (exp    (with-imported-modules '((guix build utils))
                   (gexp (begin
                           (use-modules (guix build utils))
                           (display (ungexp n))))))
         (file   (program-file "program" exp
                               #:guile %bootstrap-guile)))
    (mlet* %store-monad ((drv (lower-object file))
                         (out -> (derivation->output-path drv)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (let* ((pipe  (open-input-pipe out))
               (str   (get-string-all pipe)))
          (return (and (zero? (close-pipe pipe))
                       (= n (string->number str)))))))))

(test-assert "program-file #:module-path"
  (call-with-temporary-directory
   (lambda (directory)
     (define text (random-text))

     (call-with-output-file (string-append directory "/stupid-module.scm")
       (lambda (port)
         (write `(begin (define-module (stupid-module))
                        (define-public %stupid-thing ,text))
                port)))

     (let* ((exp    (with-imported-modules '((stupid-module))
                      (gexp (begin
                              (use-modules (stupid-module))
                              (display %stupid-thing)))))
            (file   (program-file "program" exp
                                  #:guile %bootstrap-guile
                                  #:module-path (list directory))))
       (run-with-store %store
         (mlet* %store-monad ((drv (lower-object file))
                              (out -> (derivation->output-path drv)))
           (mbegin %store-monad
             (built-derivations (list drv))
             (let* ((pipe  (open-input-pipe out))
                    (str   (get-string-all pipe)))
               (return (and (zero? (close-pipe pipe))
                            (string=? text str)))))))))))

(test-assertm "program-file & with-extensions"
  (let* ((exp    (with-extensions (list %extension-package)
                   (gexp (begin
                           (use-modules (hg2g))
                           (display the-answer)))))
         (file   (program-file "program" exp
                               #:guile %bootstrap-guile)))
    (mlet* %store-monad ((drv (lower-object file))
                         (out -> (derivation->output-path drv)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (let* ((pipe  (open-input-pipe out))
               (str   (get-string-all pipe)))
          (return (and (zero? (close-pipe pipe))
                       (= 42 (string->number str)))))))))

(test-assertm "program-file #:system"
  (let* ((exp    (with-imported-modules '((guix build utils))
                   (gexp (begin
                           (use-modules (guix build utils))
                           (display "hi!")))))
         (system (if (string=? (%current-system) "x86_64-linux")
                     "armhf-linux"
                     "x86_64-linux"))
         (file   (program-file "program" exp)))
    (mlet %store-monad ((drv (lower-object file system)))
      (return (and (string=? (derivation-system drv) system)
                   (find (lambda (input)
                           (let ((drv (pk (derivation-input-derivation input))))
                             (and (string=? (derivation-name drv)
                                            "module-import-compiled")
                                  (string=? (derivation-system drv)
                                            system))))
                         (derivation-inputs drv)))))))

(test-assertm "scheme-file"
  (let* ((text   (plain-file "foo" "Hello, world!"))
         (scheme (scheme-file "bar" #~(list "foo" #$text))))
    (mlet* %store-monad ((drv  (lower-object scheme))
                         (text (lower-object text))
                         (out -> (derivation->output-path drv)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (mlet %store-monad ((refs (references* out)))
          (return (and (equal? refs (list text))
                       (equal? `(list "foo" ,text)
                               (call-with-input-file out read)))))))))

(test-assertm "raw-derivation-file"
  (let* ((exp #~(let ((drv #$(raw-derivation-file coreutils)))
                  (when (file-exists? drv)
                    (symlink drv #$output)))))
    (mlet* %store-monad ((dep    (lower-object coreutils))
                         (drv    (gexp->derivation "drv-ref" exp))
                         (out -> (derivation->output-path drv)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (mlet %store-monad ((refs (references* out)))
          (return (and (member (derivation-file-name dep)
                               (derivation-sources drv))
                       (not (member (derivation-file-name dep)
                                    (map derivation-input-path
                                         (derivation-inputs drv))))
                       (equal? (readlink out) (derivation-file-name dep))
                       (equal? refs (list (derivation-file-name dep))))))))))

(test-assert "text-file*"
  (run-with-store %store
    (mlet* %store-monad
        ((drv  (package->derivation %bootstrap-guile))
         (guile -> (derivation->output-path drv))
         (file (text-file "bar" "This is bar."))
         (text (text-file* "foo"
                           %bootstrap-guile "/bin/guile "
                           (gexp-input %bootstrap-guile "out") "/bin/guile "
                           drv "/bin/guile "
                           file))
         (done (built-derivations (list text)))
         (out -> (derivation->output-path text))
         (refs (references* out)))
      ;; Make sure we get the right references and the right content.
      (return (and (lset= string=? refs (list guile file))
                   (equal? (call-with-input-file out get-string-all)
                           (string-append guile "/bin/guile "
                                          guile "/bin/guile "
                                          guile "/bin/guile "
                                          file)))))
    #:guile-for-build (package-derivation %store %bootstrap-guile)))

(test-assertm "mixed-text-file"
  (mlet* %store-monad ((file ->   (mixed-text-file "mixed"
                                                   "export PATH="
                                                   %bootstrap-guile "/bin"))
                       (drv       (lower-object file))
                       (out ->    (derivation->output-path drv))
                       (guile-drv (package->derivation %bootstrap-guile))
                       (guile ->  (derivation->output-path guile-drv)))
    (mbegin %store-monad
      (built-derivations (list drv))
      (mlet %store-monad ((refs (references* out)))
        (return (and (string=? (string-append "export PATH=" guile "/bin")
                               (call-with-input-file out get-string-all))
                     (equal? refs (list guile))))))))

(test-assertm "file-union"
  (mlet* %store-monad ((union -> (file-union "union"
                                             `(("a" ,(plain-file "a" "1"))
                                               ("b/c/d" ,(plain-file "d" "2"))
                                               ("e" ,(plain-file "e" "3")))))
                       (drv      (lower-object union))
                       (out ->   (derivation->output-path drv)))
    (define (contents=? file str)
      (string=? (call-with-input-file (string-append out "/" file)
                  get-string-all)
                str))

    (mbegin %store-monad
      (built-derivations (list drv))
      (return (and (contents=? "a" "1")
                   (contents=? "b/c/d" "2")
                   (contents=? "e" "3"))))))

(test-assert "gexp->derivation vs. %current-target-system"
  (let ((mval (gexp->derivation "foo"
                                #~(begin
                                    (mkdir #$output)
                                    (foo #+gnu-make))
                                #:target #f)))
    ;; The value of %CURRENT-TARGET-SYSTEM at bind-time should have no
    ;; influence.
    (parameterize ((%current-target-system "fooooo"))
      (derivation? (run-with-store %store mval)))))

(test-assertm "lower-object"
  (mlet %store-monad ((drv1 (lower-object %bootstrap-guile))
                      (drv2 (lower-object (package-source coreutils)))
                      (item (lower-object (plain-file "foo" "Hello!"))))
    (return (and (derivation? drv1) (derivation? drv2)
                 (store-path? item)))))

(test-assertm "lower-object, computed-file"
  (let* ((text     (plain-file "foo" "Hello!"))
         (exp      #~(begin
                       (mkdir #$output)
                       (symlink #$%bootstrap-guile
                                (string-append #$output "/guile"))
                       (symlink #$text (string-append #$output "/text"))))
         (computed (computed-file "computed" exp)))
    (mlet* %store-monad ((text      (lower-object text))
                         (guile-drv (lower-object %bootstrap-guile))
                         (comp-drv  (lower-object computed))
                         (comp ->   (derivation->output-path comp-drv)))
      (mbegin %store-monad
        (built-derivations (list comp-drv))
        (return (and (string=? (readlink (string-append comp "/guile"))
                               (derivation->output-path guile-drv))
                     (string=? (readlink (string-append comp "/text"))
                               text)))))))

(test-assert "lower-object, computed-file + grafts"
  ;; The reference graph should refer to grafted packages when grafts are
  ;; enabled.  See <https://issues.guix.gnu.org/50676>.
  (let* ((base    (package
                    (inherit (dummy-package "trivial"))
                    (build-system trivial-build-system)
                    (arguments
                     `(#:guile ,%bootstrap-guile
                       #:builder (mkdir %output)))))
         (pkg     (package
                    (inherit base)
                    (version "1.1")
                    (replacement (package
                                   (inherit base)
                                   (version "9.9")))))
         (exp      #~(begin
                       (use-modules (ice-9 rdelim))
                       (let ((item (call-with-input-file "graph" read-line)))
                         (call-with-output-file #$output
                           (lambda (port)
                             (display item port))))))
         (computed (computed-file "computed" exp
                                  #:options
                                  `(#:references-graphs (("graph" ,pkg)))))
         (drv0     (package-derivation %store pkg #:graft? #t))
         (drv1     (parameterize ((%graft? #t))
                     (run-with-store %store
                       (lower-object computed)))))
    (build-derivations %store (list drv1))

    ;; The graph obtained in COMPUTED should refer to the grafted version of
    ;; PKG, not to PKG itself.
    (string=? (call-with-input-file (derivation->output-path drv1)
                get-string-all)
              (derivation->output-path drv0))))

(test-equal "lower-object, computed-file, #:system"
  '("mips64el-linux")
  (run-with-store %store
    (let* ((exp      #~(symlink #$coreutils #$output))
           (computed (computed-file "computed" exp
                                    #:guile %bootstrap-guile)))
      ;; Make sure that the SYSTEM argument to 'lower-object' is honored.
      (mlet* %store-monad ((drv  (lower-object computed "mips64el-linux"))
                           (refs (references* (derivation-file-name drv))))
        (return (delete-duplicates
                 (filter-map (lambda (file)
                               (and (string-suffix? ".drv" file)
                                    (let ((drv (read-derivation-from-file
                                                file)))
                                      (derivation-system drv))))
                             (cons (derivation-file-name drv)
                                   refs))))))))

(test-assert "lower-object & gexp-input-error?"
  (guard (c ((gexp-input-error? c)
             (gexp-error-invalid-input c)))
    (run-with-store %store
      (lower-object (current-module))
      #:guile-for-build (%guile-for-build))))

(test-assert "printer"
  (string-match "^#<gexp \\(string-append .*#<package coreutils.*\
 \"/bin/uname\"\\) [[:graph:]]+tests/gexp\\.scm:[0-9]+:[0-9]+ [[:xdigit:]]+>$"
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

(test-assertm "gexp->file, cross-compilation"
  (mlet* %store-monad ((target -> "aarch64-linux-gnu")
                       (exp    -> (gexp (list (ungexp coreutils))))
                       (xdrv      (gexp->file "foo" exp #:target target))
                       (refs      (references*
                                   (derivation-file-name xdrv)))
                       (xcu       (package->cross-derivation coreutils
                                                             target))
                       (cu        (package->derivation coreutils)))
    (return (and (member (derivation-file-name xcu) refs)
                 (not (member (derivation-file-name cu) refs))))))

(test-assertm "gexp->file, cross-compilation with default target"
  (mlet* %store-monad ((target -> "aarch64-linux-gnu")
                       (_         (set-current-target target))
                       (exp    -> (gexp (list (ungexp coreutils))))
                       (xdrv      (gexp->file "foo" exp))
                       (refs      (references*
                                   (derivation-file-name xdrv)))
                       (xcu       (package->cross-derivation coreutils
                                                             target))
                       (cu        (package->derivation coreutils)))
    (return (and (member (derivation-file-name xcu) refs)
                 (not (member (derivation-file-name cu) refs))))))

(test-assertm "gexp->script, cross-compilation"
  (mlet* %store-monad ((target -> "aarch64-linux-gnu")
                       (exp    -> (gexp (list (ungexp coreutils))))
                       (xdrv      (gexp->script "foo" exp #:target target))
                       (refs      (references*
                                   (derivation-file-name xdrv)))
                       (xcu       (package->cross-derivation coreutils
                                                             target))
                       (cu        (package->derivation coreutils)))
    (return (and (member (derivation-file-name xcu) refs)
                 (not (member (derivation-file-name cu) refs))))))

(test-assertm "gexp->script, cross-compilation with default target"
  (mlet* %store-monad ((target -> "aarch64-linux-gnu")
                       (_         (set-current-target target))
                       (exp    -> (gexp (list (ungexp coreutils))))
                       (xdrv      (gexp->script "foo" exp))
                       (refs      (references*
                                   (derivation-file-name xdrv)))
                       (xcu       (package->cross-derivation coreutils
                                                             target))
                       (cu        (package->derivation coreutils)))
    (return (and (member (derivation-file-name xcu) refs)
                 (not (member (derivation-file-name cu) refs))))))

(test-end "gexp")

;; Local Variables:
;; eval: (put 'test-assertm 'scheme-indent-function 1)
;; End:
