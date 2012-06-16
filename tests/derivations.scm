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


(define-module (test-derivations)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match))

(define %store
  (false-if-exception (open-connection)))

(define (directory-contents dir)
  "Return an alist representing the contents of DIR."
  (define prefix-len (string-length dir))
  (sort (file-system-fold (const #t)                   ; enter?
                          (lambda (path stat result)   ; leaf
                            (alist-cons (string-drop path prefix-len)
                                        (call-with-input-file path
                                          get-bytevector-all)
                                        result))
                          (lambda (path stat result) result)      ; down
                          (lambda (path stat result) result)      ; up
                          (lambda (path stat result) result)      ; skip
                          (lambda (path stat errno result) result) ; error
                          '()
                          dir)
        (lambda (e1 e2)
          (string<? (car e1) (car e2)))))

(test-begin "derivations")

(test-assert "parse & export"
  (let* ((f  (search-path %load-path "tests/test.drv"))
         (b1 (call-with-input-file f get-bytevector-all))
         (d1 (read-derivation (open-bytevector-input-port b1)))
         (b2 (call-with-bytevector-output-port (cut write-derivation d1 <>)))
         (d2 (read-derivation (open-bytevector-input-port b2))))
    (and (equal? b1 b2)
         (equal? d1 d2))))

(test-skip (if %store 0 4))

(test-assert "add-to-store, flat"
  (let* ((file (search-path %load-path "language/tree-il/spec.scm"))
         (drv  (add-to-store %store "flat-test" #t #f "sha256" file)))
    (and (eq? 'regular (stat:type (stat drv)))
         (equal? (call-with-input-file file get-bytevector-all)
                 (call-with-input-file drv get-bytevector-all)))))

(test-assert "add-to-store, recursive"
  (let* ((dir (dirname (search-path %load-path "language/tree-il/spec.scm")))
         (drv (add-to-store %store "dir-tree-test" #t #t "sha256" dir)))
    (and (eq? 'directory (stat:type (stat drv)))
         (equal? (directory-contents dir)
                 (directory-contents drv)))))

(test-assert "derivation with no inputs"
  (let ((builder (add-text-to-store %store "my-builder.sh"
                                    "#!/bin/sh\necho hello, world\n"
                                    '())))
    (store-path? (derivation %store "foo" (%current-system) builder
                             '() '(("HOME" . "/homeless")) '()))))

(test-assert "build derivation with 1 source"
  (let*-values (((builder)
                 (add-text-to-store %store "my-builder.sh"
                                    "echo hello, world > \"$out\"\n"
                                    '()))
                ((drv-path drv)
                 (derivation %store "foo" (%current-system)
                             "/bin/sh" `(,builder)
                             '(("HOME" . "/homeless")
                               ("zzz"  . "Z!")
                               ("AAA"  . "A!"))
                             `((,builder))))
                ((succeeded?)
                 (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((path (derivation-output-path
                      (assoc-ref (derivation-outputs drv) "out"))))
           (string=? (call-with-input-file path read-line)
                     "hello, world")))))

(test-assert "fixed-output derivation"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo -n hello > $out" '()))
         (hash       (sha256 (string->utf8 "hello")))
         (drv-path   (derivation %store "fixed" (%current-system)
                                 "/bin/sh" `(,builder)
                                 '() `((,builder))
                                 #:hash hash #:hash-algo 'sha256))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((p (derivation-path->output-path drv-path)))
           (equal? (string->utf8 "hello")
                   (call-with-input-file p get-bytevector-all))))))

(test-assert "multiple-output derivation"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo one > $out ; echo two > $second"
                                        '()))
         (drv-path   (derivation %store "fixed" (%current-system)
                                 "/bin/sh" `(,builder)
                                 '(("HOME" . "/homeless")
                                   ("zzz"  . "Z!")
                                   ("AAA"  . "A!"))
                                 `((,builder))
                                 #:outputs '("out" "second")))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((one (derivation-path->output-path drv-path "out"))
               (two (derivation-path->output-path drv-path "second")))
           (and (eq? 'one (call-with-input-file one read))
                (eq? 'two (call-with-input-file two read)))))))


(define %coreutils
  (false-if-exception (nixpkgs-derivation "coreutils")))

(test-skip (if %coreutils 0 1))

(test-assert "build derivation with coreutils"
  (let* ((builder
          (add-text-to-store %store "build-with-coreutils.sh"
                             "echo $PATH ; mkdir --version ; mkdir $out ; touch $out/good"
                             '()))
         (drv-path
          (derivation %store "foo" (%current-system)
                      "/bin/sh" `(,builder)
                      `(("PATH" .
                         ,(string-append
                           (derivation-path->output-path %coreutils)
                           "/bin")))
                      `((,builder)
                        (,%coreutils))))
         (succeeded?
          (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((p (derivation-path->output-path drv-path)))
           (file-exists? (string-append p "/good"))))))

(test-skip (if (%guile-for-build) 0 4))

(test-assert "build-expression->derivation without inputs"
  (let* ((builder    '(begin
                        (mkdir %output)
                        (call-with-output-file (string-append %output "/test")
                          (lambda (p)
                            (display '(hello guix) p)))))
         (drv-path   (build-expression->derivation %store "goo" (%current-system)
                                                   builder '()))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((p (derivation-path->output-path drv-path)))
           (equal? '(hello guix)
                   (call-with-input-file (string-append p "/test") read))))))

(test-assert "build-expression->derivation with expression returning #f"
  (let* ((builder  '(begin
                      (mkdir %output)
                      #f))                        ; fail!
         (drv-path (build-expression->derivation %store "fail" (%current-system)
                                                 builder '())))
    (guard (c ((nix-protocol-error? c)
               ;; Note that the output path may exist at this point, but it
               ;; is invalid.
               (not (not (string-match "build .* failed"
                                       (nix-protocol-error-message c))))))
      (build-derivations %store (list drv-path))
      #f)))

(test-assert "build-expression->derivation with two outputs"
  (let* ((builder    '(begin
                        (call-with-output-file (assoc-ref %outputs "out")
                          (lambda (p)
                            (display '(hello) p)))
                        (call-with-output-file (assoc-ref %outputs "second")
                          (lambda (p)
                            (display '(world) p)))))
         (drv-path   (build-expression->derivation %store "double"
                                                   (%current-system)
                                                   builder '()
                                                   #:outputs '("out"
                                                               "second")))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((one (derivation-path->output-path drv-path))
               (two (derivation-path->output-path drv-path "second")))
           (and (equal? '(hello) (call-with-input-file one read))
                (equal? '(world) (call-with-input-file two read)))))))

(test-assert "build-expression->derivation with one input"
  (let* ((builder    '(call-with-output-file %output
                        (lambda (p)
                          (let ((cu (assoc-ref %build-inputs "cu")))
                            (close 1)
                            (dup2 (port->fdes p) 1)
                            (execl (string-append cu "/bin/uname")
                                   "uname" "-a")))))
         (drv-path   (build-expression->derivation %store "uname" (%current-system)
                                                   builder
                                                   `(("cu" ,%coreutils))))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((p (derivation-path->output-path drv-path)))
           (string-contains (call-with-input-file p read-line) "GNU")))))

(test-assert "imported-files"
  (let* ((files    `(("x"     . ,(search-path %load-path "ice-9/q.scm"))
                     ("a/b/c" . ,(search-path %load-path
                                              "guix/derivations.scm"))
                     ("p/q"   . ,(search-path %load-path "guix.scm"))
                     ("p/z"   . ,(search-path %load-path "guix/store.scm"))))
         (drv-path (imported-files %store files)))
    (and (build-derivations %store (list drv-path))
         (let ((dir (derivation-path->output-path drv-path)))
           (every (match-lambda
                   ((path . source)
                    (equal? (call-with-input-file (string-append dir "/" path)
                              get-bytevector-all)
                            (call-with-input-file source
                              get-bytevector-all))))
                  files)))))

(test-skip (if (false-if-exception (getaddrinfo "ftp.gnu.org" "http"))
               0
               1))

(test-assert "build-expression->derivation for fixed-output derivation"
  (let* ((url         "http://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz")
         (builder
          `(begin
             (use-modules (web client) (web uri)
                          (rnrs io ports) (srfi srfi-11))
             (let-values (((resp bv)
                           (http-get (string->uri ,url) #:decode-body? #f)))
               (call-with-output-file %output
                 (lambda (p)
                   (put-bytevector p bv))))))
         (drv-path    (build-expression->derivation
                       %store "hello-2.8.tar.gz" (%current-system) builder '()
                       #:hash (nix-base32-string->bytevector
                               "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6")
                       #:hash-algo 'sha256))
         (succeeded?  (build-derivations %store (list drv-path))))
    (and succeeded?
         (file-exists? (derivation-path->output-path drv-path)))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; eval: (put 'guard 'scheme-indent-function 1)
;;; End:
