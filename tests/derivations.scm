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
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 rdelim))

(define %store
  (false-if-exception (open-connection)))

(test-begin "derivations")

(test-assert "parse & export"
  (let* ((f  (search-path %load-path "tests/test.drv"))
         (b1 (call-with-input-file f get-bytevector-all))
         (d1 (read-derivation (open-bytevector-input-port b1)))
         (b2 (call-with-bytevector-output-port (cut write-derivation d1 <>)))
         (d2 (read-derivation (open-bytevector-input-port b2))))
    (and (equal? b1 b2)
         (equal? d1 d2))))

(test-skip (if %store 0 3))

(test-assert "derivation with no inputs"
  (let ((builder (add-text-to-store %store "my-builder.sh"
                                    "#!/bin/sh\necho hello, world\n"
                                    '())))
    (store-path? (derivation %store "foo" "x86_64-linux" builder
                             '() '(("HOME" . "/homeless")) '()))))

(test-assert "build derivation with 1 source"
  (let*-values (((builder)
                 (add-text-to-store %store "my-builder.sh"
                                    "echo hello, world > \"$out\"\n"
                                    '()))
                ((drv-path drv)
                 (derivation %store "foo" "x86_64-linux"
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
         (drv-path   (derivation %store "fixed" "x86_64-linux"
                                 "/bin/sh" `(,builder)
                                 '() `((,builder))
                                 #:hash hash #:hash-algo 'sha256))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((p (derivation-path->output-path drv-path)))
           (equal? (string->utf8 "hello")
                   (call-with-input-file p get-bytevector-all))))))


(define %coreutils
  (false-if-exception (nixpkgs-derivation "coreutils")))

(test-skip (if %coreutils 0 1))

(test-assert "build derivation with coreutils"
  (let* ((builder
          (add-text-to-store %store "build-with-coreutils.sh"
                             "echo $PATH ; mkdir --version ; mkdir $out ; touch $out/good"
                             '()))
         (drv-path
          (derivation %store "foo" "x86_64-linux"
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

(test-skip (if (%guile-for-build) 0 2))

(test-assert "build-expression->derivation without inputs"
  (let* ((builder    '(begin
                        (mkdir %output)
                        (call-with-output-file (string-append %output "/test")
                          (lambda (p)
                            (display '(hello guix) p)))))
         (drv-path   (build-expression->derivation %store "goo" "x86_64-linux"
                                                   builder '()))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((p (derivation-path->output-path drv-path)))
           (equal? '(hello guix)
                   (call-with-input-file (string-append p "/test") read))))))

(test-assert "build-expression->derivation with one input"
  (let* ((builder    '(call-with-output-file %output
                        (lambda (p)
                          (let ((cu (assoc-ref %build-inputs "cu")))
                            (close 1)
                            (dup2 (port->fdes p) 1)
                            (execl (string-append cu "/bin/uname")
                                   "uname" "-a")))))
         (drv-path   (build-expression->derivation %store "uname" "x86_64-linux"
                                                   builder
                                                   `(("cu" . ,%coreutils))))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((p (derivation-path->output-path drv-path)))
           (string-contains (call-with-input-file p read-line) "GNU")))))

(test-skip (if (false-if-exception (getaddrinfo "ftp.gnu.org" "http"))
               0
               1))

(test-assert "build-expression->derivation for fixed-output derivation"
  (let* ((url         "http://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz")
         (builder     `(begin
                         (use-modules (web client) (web uri)
                                      (rnrs io ports))
                         (let ((bv (http-get (string->uri ,url)
                                             #:decode-body? #f)))
                           (call-with-output-file %output
                             (lambda (p)
                               (put-bytevector p bv))))))
         (drv-path    (build-expression->derivation
                       %store "hello-2.8.tar.gz" "x86_64-linux" builder '()
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
;;; End:
