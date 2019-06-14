;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-grafts)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix grafts)
  #:use-module (guix tests)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 vlist))

(define %store
  (open-connection-for-tests))

(define (bootstrap-binary name)
  (let ((bin (search-bootstrap-binary name (%current-system))))
    (and %store
         (add-to-store %store name #t "sha256" bin))))

(define %bash
  (bootstrap-binary "bash"))
(define %mkdir
  (bootstrap-binary "mkdir"))

(define make-derivation-input
  (@@ (guix derivations) make-derivation-input))


(test-begin "grafts")

(test-equal "graft-derivation, grafted item is a direct dependency"
  '((type . graft) (graft (count . 2)))
  (let* ((build `(begin
                   (mkdir %output)
                   (chdir %output)
                   (symlink %output "self")
                   (call-with-output-file "text"
                     (lambda (output)
                       (format output "foo/~a/bar" ,%mkdir)))
                   (symlink ,%bash "sh")))
         (orig  (build-expression->derivation %store "grafted" build
                                              #:inputs `(("a" ,%bash)
                                                         ("b" ,%mkdir))))
         (one   (add-text-to-store %store "bash" "fake bash"))
         (two   (build-expression->derivation %store "mkdir"
                                              '(call-with-output-file %output
                                                 (lambda (port)
                                                   (display "fake mkdir" port)))))
         (grafted (graft-derivation %store orig
                                    (list (graft
                                            (origin %bash)
                                            (replacement one))
                                          (graft
                                            (origin %mkdir)
                                            (replacement two))))))
    (and (build-derivations %store (list grafted))
         (let ((properties (derivation-properties grafted))
               (two        (derivation->output-path two))
               (grafted    (derivation->output-path grafted)))
           (and (string=? (format #f "foo/~a/bar" two)
                          (call-with-input-file (string-append grafted "/text")
                            get-string-all))
                (string=? (readlink (string-append grafted "/sh")) one)
                (string=? (readlink (string-append grafted "/self"))
                          grafted)
                properties)))))

(test-assert "graft-derivation, grafted item uses a different name"
  (let* ((build   `(begin
                     (mkdir %output)
                     (chdir %output)
                     (symlink %output "self")
                     (symlink ,%bash "sh")))
         (orig    (build-expression->derivation %store "grafted" build
                                                #:inputs `(("a" ,%bash))))
         (repl    (add-text-to-store %store "BaSH" "fake bash"))
         (grafted (graft-derivation %store orig
                                    (list (graft
                                            (origin %bash)
                                            (replacement repl))))))
    (and (build-derivations %store (list grafted))
         (let ((grafted (derivation->output-path grafted)))
           (and (string=? (readlink (string-append grafted "/sh")) repl)
                (string=? (readlink (string-append grafted "/self"))
                          grafted))))))

;; Make sure 'derivation-file-name' always gets to see an absolute file name.
(fluid-set! %file-port-name-canonicalization 'absolute)

(test-assert "graft-derivation, grafted item is an indirect dependency"
  (let* ((build `(begin
                   (mkdir %output)
                   (chdir %output)
                   (symlink %output "self")
                   (call-with-output-file "text"
                     (lambda (output)
                       (format output "foo/~a/bar" ,%mkdir)))
                   (symlink ,%bash "sh")))
         (dep   (build-expression->derivation %store "dep" build
                                              #:inputs `(("a" ,%bash)
                                                         ("b" ,%mkdir))))
         (orig  (build-expression->derivation %store "thing"
                                              '(symlink
                                                (assoc-ref %build-inputs
                                                           "dep")
                                                %output)
                                              #:inputs `(("dep" ,dep))))
         (one   (add-text-to-store %store "bash" "fake bash"))
         (two   (build-expression->derivation %store "mkdir"
                                              '(call-with-output-file %output
                                                 (lambda (port)
                                                   (display "fake mkdir" port)))))
         (grafted (graft-derivation %store orig
                                    (list (graft
                                            (origin %bash)
                                            (replacement one))
                                          (graft
                                            (origin %mkdir)
                                            (replacement two))))))
    (and (build-derivations %store (list grafted))
         (let* ((two     (derivation->output-path two))
                (grafted (derivation->output-path grafted))
                (dep     (readlink grafted)))
           (and (string=? (format #f "foo/~a/bar" two)
                          (call-with-input-file (string-append dep "/text")
                            get-string-all))
                (string=? (readlink (string-append dep "/sh")) one)
                (string=? (readlink (string-append dep "/self")) dep)
                (equal? (references %store grafted) (list dep))
                (lset= string=?
                       (list one two dep)
                       (references %store dep)))))))

(test-assert "graft-derivation, preserve empty directories"
  (run-with-store %store
    (mlet* %store-monad ((fake    (text-file "bash" "Fake bash."))
                         (graft -> (graft
                                     (origin %bash)
                                     (replacement fake)))
                         (drv     (gexp->derivation
                                   "to-graft"
                                   (with-imported-modules '((guix build utils))
                                     #~(begin
                                         (use-modules (guix build utils))
                                         (mkdir-p (string-append #$output
                                                                 "/a/b/c/d"))
                                         (symlink #$%bash
                                                  (string-append #$output
                                                                 "/bash"))))))
                         (grafted ((store-lift graft-derivation) drv
                                   (list graft)))
                         (_       (built-derivations (list grafted)))
                         (out ->  (derivation->output-path grafted)))
      (return (and (string=? (readlink (string-append out "/bash"))
                             fake)
                   (file-is-directory? (string-append out "/a/b/c/d")))))))

(test-assert "graft-derivation, no dependencies on grafted output"
  (run-with-store %store
    (mlet* %store-monad ((fake    (text-file "bash" "Fake bash."))
                         (graft -> (graft
                                     (origin %bash)
                                     (replacement fake)))
                         (drv     (gexp->derivation "foo" #~(mkdir #$output)))
                         (grafted ((store-lift graft-derivation) drv
                                   (list graft))))
      (return (eq? grafted drv)))))

(test-assert "graft-derivation, multiple outputs"
  (let* ((build `(begin
                   (symlink (assoc-ref %build-inputs "a")
                            (assoc-ref %outputs "one"))
                   (symlink (assoc-ref %outputs "one")
                            (assoc-ref %outputs "two"))))
         (orig  (build-expression->derivation %store "grafted" build
                                              #:inputs `(("a" ,%bash))
                                              #:outputs '("one" "two")))
         (repl  (add-text-to-store %store "bash" "fake bash"))
         (grafted (graft-derivation %store orig
                                    (list (graft
                                            (origin %bash)
                                            (replacement repl))))))
    (and (build-derivations %store (list grafted))
         (let ((one (derivation->output-path grafted "one"))
               (two (derivation->output-path grafted "two")))
           (and (string=? (readlink one) repl)
                (string=? (readlink two) one))))))

(test-assert "graft-derivation, replaced derivation has multiple outputs"
  ;; Here we have a replacement just for output "one" of P1 and not for the
  ;; other output.  Make sure the graft for P1:one correctly applies to the
  ;; dependents of P1.  See <http://bugs.gnu.org/24712>.
  (let* ((p1  (build-expression->derivation
               %store "p1"
               `(let ((one (assoc-ref %outputs "one"))
                      (two (assoc-ref %outputs "two")))
                  (mkdir one)
                  (mkdir two))
               #:outputs '("one" "two")))
         (p1r (build-expression->derivation
               %store "P1"
               `(let ((other (assoc-ref %outputs "ONE")))
                  (mkdir other)
                  (call-with-output-file (string-append other "/replacement")
                    (const #t)))
               #:outputs '("ONE")))
         (p2  (build-expression->derivation
               %store "p2"
               `(let ((out (assoc-ref %outputs "aaa")))
                  (mkdir (assoc-ref %outputs "zzz"))
                  (mkdir out) (chdir out)
                  (symlink (assoc-ref %build-inputs "p1:one") "one")
                  (symlink (assoc-ref %build-inputs "p1:two") "two"))
               #:outputs '("aaa" "zzz")
               #:inputs `(("p1:one" ,p1 "one")
                          ("p1:two" ,p1 "two"))))
         (p3  (build-expression->derivation
               %store "p3"
               `(symlink (assoc-ref %build-inputs "p2:aaa")
                         (assoc-ref %outputs "out"))
               #:inputs `(("p2:aaa" ,p2 "aaa")
                          ("p2:zzz" ,p2 "zzz"))))
         (p1g (graft
                (origin p1)
                (origin-output "one")
                (replacement p1r)
                (replacement-output "ONE")))
         (p3d (graft-derivation %store p3 (list p1g))))

    (and (not (find (lambda (input)
                      ;; INPUT should not be P2:zzz since the result of P3
                      ;; does not depend on it.  See
                      ;; <http://bugs.gnu.org/24886>.
                      (and (string=? (derivation-input-path input)
                                     (derivation-file-name p2))
                           (member "zzz"
                                   (derivation-input-sub-derivations input))))
                    (derivation-inputs p3d)))

         (build-derivations %store (list p3d))
         (let ((out (derivation->output-path (pk 'p2d p3d))))
           (and (not (string=? (readlink out)
                               (derivation->output-path p2 "aaa")))
                (string=? (derivation->output-path p1 "two")
                          (readlink (string-append out "/two")))
                (file-exists? (string-append out "/one/replacement")))))))

(test-assert "graft-derivation with #:outputs"
  ;; Call 'graft-derivation' with a narrowed set of outputs passed as
  ;; #:outputs.
  (let* ((p1  (build-expression->derivation
               %store "p1"
               `(let ((one (assoc-ref %outputs "one"))
                      (two (assoc-ref %outputs "two")))
                  (mkdir one)
                  (mkdir two))
               #:outputs '("one" "two")))
         (p1r (build-expression->derivation
               %store "P1"
               `(let ((other (assoc-ref %outputs "ONE")))
                  (mkdir other)
                  (call-with-output-file (string-append other "/replacement")
                    (const #t)))
               #:outputs '("ONE")))
         (p2  (build-expression->derivation
               %store "p2"
               `(let ((aaa (assoc-ref %outputs "aaa"))
                      (zzz (assoc-ref %outputs "zzz")))
                  (mkdir zzz) (chdir zzz)
                  (mkdir aaa) (chdir aaa)
                  (symlink (assoc-ref %build-inputs "p1:two") "two"))
               #:outputs '("aaa" "zzz")
               #:inputs `(("p1:one" ,p1 "one")
                          ("p1:two" ,p1 "two"))))
         (p1g (graft
                (origin p1)
                (origin-output "one")
                (replacement p1r)
                (replacement-output "ONE")))
         (p2g (graft-derivation %store p2 (list p1g)
                                #:outputs '("aaa"))))
    ;; P2:aaa depends on P1:two, but not on P1:one, so nothing to graft.
    (eq? p2g p2)))

(test-equal "graft-derivation, unused outputs not depended on"
  '("aaa")

  ;; Make sure that the result of 'graft-derivation' does not pull outputs
  ;; that are irrelevant to the grafting process.  See
  ;; <http://bugs.gnu.org/24886>.
  (let* ((p1  (build-expression->derivation
               %store "p1"
               `(let ((one (assoc-ref %outputs "one"))
                      (two (assoc-ref %outputs "two")))
                  (mkdir one)
                  (mkdir two))
               #:outputs '("one" "two")))
         (p1r (build-expression->derivation
               %store "P1"
               `(let ((other (assoc-ref %outputs "ONE")))
                  (mkdir other)
                  (call-with-output-file (string-append other "/replacement")
                    (const #t)))
               #:outputs '("ONE")))
         (p2  (build-expression->derivation
               %store "p2"
               `(let ((aaa (assoc-ref %outputs "aaa"))
                      (zzz (assoc-ref %outputs "zzz")))
                  (mkdir zzz) (chdir zzz)
                  (symlink (assoc-ref %build-inputs "p1:two") "two")
                  (mkdir aaa) (chdir aaa)
                  (symlink (assoc-ref %build-inputs "p1:one") "one"))
               #:outputs '("aaa" "zzz")
               #:inputs `(("p1:one" ,p1 "one")
                          ("p1:two" ,p1 "two"))))
         (p1g (graft
                (origin p1)
                (origin-output "one")
                (replacement p1r)
                (replacement-output "ONE")))
         (p2g (graft-derivation %store p2 (list p1g)
                                #:outputs '("aaa"))))

    ;; Here P2G should only depend on P1:one and P1R:one; it must not depend
    ;; on P1:two or P1R:two since these are unused in the grafting process.
    (and (not (eq? p2g p2))
         (let* ((inputs      (derivation-inputs p2g))
                (match-input (lambda (drv)
                               (lambda (input)
                                 (string=? (derivation-input-path input)
                                           (derivation-file-name drv)))))
                (p1-inputs   (filter (match-input p1) inputs))
                (p1r-inputs  (filter (match-input p1r) inputs))
                (p2-inputs   (filter (match-input p2) inputs)))
           (and (equal? p1-inputs
                        (list (make-derivation-input (derivation-file-name p1)
                                                     '("one"))))
                (equal? p1r-inputs
                        (list
                         (make-derivation-input (derivation-file-name p1r)
                                                '("ONE"))))
                (equal? p2-inputs
                        (list
                         (make-derivation-input (derivation-file-name p2)
                                                '("aaa"))))
                (derivation-output-names p2g))))))

(test-assert "graft-derivation, renaming"         ;<http://bugs.gnu.org/23132>
  (let* ((build `(begin
                   (use-modules (guix build utils))
                   (mkdir-p (string-append (assoc-ref %outputs "out") "/"
                                           (assoc-ref %build-inputs "in")))))
         (orig  (build-expression->derivation %store "thing-to-graft" build
                                              #:modules '((guix build utils))
                                              #:inputs `(("in" ,%bash))))
         (repl  (add-text-to-store %store "bash" "fake bash"))
         (grafted (graft-derivation %store orig
                                    (list (graft
                                            (origin %bash)
                                            (replacement repl))))))
    (and (build-derivations %store (list grafted))
         (let ((out (derivation->output-path grafted)))
           (file-is-directory? (string-append out "/" repl))))))

(test-assert "graft-derivation, grafts are not shadowed"
  ;; We build a DAG as below, where dotted arrows represent replacements and
  ;; solid arrows represent dependencies:
  ;;
  ;;  P1  ·············>  P1R
  ;;  |\__________________.
  ;;  v                   v
  ;;  P2  ·············>  P2R
  ;;  |
  ;;  v
  ;;  P3
  ;;
  ;; We want to make sure that the two grafts we want to apply to P3 are
  ;; honored and not shadowed by other computed grafts.
  (let* ((p1     (build-expression->derivation
                  %store "p1"
                  '(mkdir (assoc-ref %outputs "out"))))
         (p1r    (build-expression->derivation
                  %store "P1"
                  '(let ((out (assoc-ref %outputs "out")))
                     (mkdir out)
                     (call-with-output-file (string-append out "/replacement")
                       (const #t)))))
         (p2     (build-expression->derivation
                  %store "p2"
                  `(let ((out (assoc-ref %outputs "out")))
                     (mkdir out)
                     (chdir out)
                     (symlink (assoc-ref %build-inputs "p1") "p1"))
                  #:inputs `(("p1" ,p1))))
         (p2r    (build-expression->derivation
                  %store "P2"
                  `(let ((out (assoc-ref %outputs "out")))
                     (mkdir out)
                     (chdir out)
                     (symlink (assoc-ref %build-inputs "p1") "p1")
                     (call-with-output-file (string-append out "/replacement")
                       (const #t)))
                  #:inputs `(("p1" ,p1))))
         (p3     (build-expression->derivation
                  %store "p3"
                  `(let ((out (assoc-ref %outputs "out")))
                     (mkdir out)
                     (chdir out)
                     (symlink (assoc-ref %build-inputs "p2") "p2"))
                  #:inputs `(("p2" ,p2))))
         (p1g    (graft
                   (origin p1)
                   (replacement p1r)))
         (p2g    (graft
                   (origin p2)
                   (replacement (graft-derivation %store p2r (list p1g)))))
         (p3d    (graft-derivation %store p3 (list p1g p2g))))
    (and (build-derivations %store (list p3d))
         (let ((out (derivation->output-path (pk p3d))))
           ;; Make sure OUT refers to the replacement of P2, which in turn
           ;; refers to the replacement of P1, as specified by P1G and P2G.
           ;; It used to be the case that P2G would be shadowed by a simple
           ;; P2->P2R graft, which is not what we want.
           (and (file-exists? (string-append out "/p2/replacement"))
                (file-exists? (string-append out "/p2/p1/replacement")))))))

(define buffer-size
  ;; Must be equal to REQUEST-SIZE in 'replace-store-references'.
  (expt 2 20))

(test-equal "replace-store-references, <http://bugs.gnu.org/28212>"
  (string-append (make-string (- buffer-size 47) #\a)
                 "/gnu/store/" (make-string 32 #\8)
                 "-SoMeTHiNG"
                 (list->string (map integer->char (iota 77 33))))

  ;; Create input data where the right-hand-size of the dash ("-something"
  ;; here) goes beyond the end of the internal buffer of
  ;; 'replace-store-references'.
  (let* ((content     (string-append (make-string (- buffer-size 47) #\a)
                                     "/gnu/store/" (make-string 32 #\7)
                                     "-something"
                                     (list->string
                                      (map integer->char (iota 77 33)))))
         (replacement (alist->vhash
                       `((,(make-string 32 #\7)
                          . ,(string->utf8 (string-append
                                            (make-string 32 #\8)
                                            "-SoMeTHiNG")))))))
    (call-with-output-string
      (lambda (output)
        ((@@ (guix build graft) replace-store-references)
         (open-input-string content) output
         replacement
         "/gnu/store")))))

(test-end)
