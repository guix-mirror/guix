;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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


(define-module (test-store)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix nar)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;; Test the (guix store) module.

(define %store
  (false-if-exception (open-connection)))

(when %store
  ;; Make sure we build everything by ourselves.
  (set-build-options %store #:use-substitutes? #f))

(define %seed
  (seed->random-state (logxor (getpid) (car (gettimeofday)))))

(define (random-text)
  (number->string (random (expt 2 256) %seed) 16))


(test-begin "store")

(test-equal "store-path-hash-part"
  "283gqy39v3g9dxjy26rynl0zls82fmcg"
  (store-path-hash-part
   (string-append (%store-prefix)
                  "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7")))

(test-equal "store-path-hash-part #f"
  #f
  (store-path-hash-part
   (string-append (%store-prefix)
                  "/foo/bar/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7")))

(test-assert "direct-store-path?"
  (and (direct-store-path?
        (string-append (%store-prefix)
                       "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7"))
       (not (direct-store-path?
             (string-append
              (%store-prefix)
              "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7/bin/guile")))))

(test-skip (if %store 0 10))

(test-assert "dead-paths"
  (let ((p (add-text-to-store %store "random-text" (random-text))))
    (member p (dead-paths %store))))

;; FIXME: Find a test for `live-paths'.
;;
;; (test-assert "temporary root is in live-paths"
;;   (let* ((p1 (add-text-to-store %store "random-text"
;;                                 (random-text) '()))
;;          (b  (add-text-to-store %store "link-builder"
;;                                 (format #f "echo ~a > $out" p1)
;;                                 '()))
;;          (d1 (derivation %store "link"
;;                          "/bin/sh" `("-e" ,b)
;;                          #:inputs `((,b) (,p1))))
;;          (p2 (derivation->output-path d1)))
;;     (and (add-temp-root %store p2)
;;          (build-derivations %store (list d1))
;;          (valid-path? %store p1)
;;          (member (pk p2) (live-paths %store)))))

(test-assert "dead path can be explicitly collected"
  (let ((p (add-text-to-store %store "random-text"
                              (random-text) '())))
    (let-values (((paths freed) (delete-paths %store (list p))))
      (and (equal? paths (list p))
           (> freed 0)
           (not (file-exists? p))))))

(test-assert "references"
  (let* ((t1 (add-text-to-store %store "random1"
                                (random-text)))
         (t2 (add-text-to-store %store "random2"
                                (random-text) (list t1))))
    (and (equal? (list t1) (references %store t2))
         (equal? (list t2) (referrers %store t1))
         (null? (references %store t1))
         (null? (referrers %store t2)))))

(test-assert "requisites"
  (let* ((t1 (add-text-to-store %store "random1"
                                (random-text) '()))
         (t2 (add-text-to-store %store "random2"
                                (random-text) (list t1)))
         (t3 (add-text-to-store %store "random3"
                                (random-text) (list t2)))
         (t4 (add-text-to-store %store "random4"
                                (random-text) (list t1 t3))))
    (define (same? x y)
      (and (= (length x) (length y))
           (lset= equal? x y)))

    (and (same? (requisites %store t1) (list t1))
         (same? (requisites %store t2) (list t1 t2))
         (same? (requisites %store t3) (list t1 t2 t3))
         (same? (requisites %store t4) (list t1 t2 t3 t4)))))

(test-assert "derivers"
  (let* ((b (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s (add-to-store %store "bash" #t "sha256"
                          (search-bootstrap-binary "bash"
                                                   (%current-system))))
         (d (derivation %store "the-thing"
                        s `("-e" ,b)
                        #:env-vars `(("foo" . ,(random-text)))
                        #:inputs `((,b) (,s))))
         (o (derivation->output-path d)))
    (and (build-derivations %store (list d))
         (equal? (query-derivation-outputs %store (derivation-file-name d))
                 (list o))
         (equal? (valid-derivers %store o)
                 (list (derivation-file-name d))))))

(test-assert "log-file, derivation"
  (let* ((b (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s (add-to-store %store "bash" #t "sha256"
                          (search-bootstrap-binary "bash"
                                                   (%current-system))))
         (d (derivation %store "the-thing"
                        s `("-e" ,b)
                        #:env-vars `(("foo" . ,(random-text)))
                        #:inputs `((,b) (,s)))))
    (and (build-derivations %store (list d))
         (file-exists? (pk (log-file %store (derivation-file-name d)))))))

(test-assert "log-file, output file name"
  (let* ((b (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s (add-to-store %store "bash" #t "sha256"
                          (search-bootstrap-binary "bash"
                                                   (%current-system))))
         (d (derivation %store "the-thing"
                        s `("-e" ,b)
                        #:env-vars `(("foo" . ,(random-text)))
                        #:inputs `((,b) (,s))))
         (o (derivation->output-path d)))
    (and (build-derivations %store (list d))
         (file-exists? (pk (log-file %store o)))
         (string=? (log-file %store (derivation-file-name d))
                   (log-file %store o)))))

(test-assert "no substitutes"
  (let* ((s  (open-connection))
         (d1 (package-derivation s %bootstrap-guile (%current-system)))
         (d2 (package-derivation s %bootstrap-glibc (%current-system)))
         (o  (map derivation->output-path (list d1 d2))))
    (set-build-options s #:use-substitutes? #f)
    (and (not (has-substitutes? s (derivation-file-name d1)))
         (not (has-substitutes? s (derivation-file-name d2)))
         (null? (substitutable-paths s o))
         (null? (substitutable-path-info s o)))))

(test-skip (if (getenv "GUIX_BINARY_SUBSTITUTE_URL") 0 1))

(test-assert "substitute query"
  (let* ((s   (open-connection))
         (d   (package-derivation s %bootstrap-guile (%current-system)))
         (o   (derivation->output-path d))
         (dir (and=> (getenv "GUIX_BINARY_SUBSTITUTE_URL")
                     (compose uri-path string->uri))))
    ;; Create fake substituter data, to be read by `substitute-binary'.
    (call-with-output-file (string-append dir "/nix-cache-info")
      (lambda (p)
        (format p "StoreDir: ~a\nWantMassQuery: 0\n"
                (%store-prefix))))
    (call-with-output-file (string-append dir "/" (store-path-hash-part o)
                                          ".narinfo")
      (lambda (p)
        (format p "StorePath: ~a
URL: ~a
Compression: none
NarSize: 1234
References: 
System: ~a
Deriver: ~a~%"
                o                                   ; StorePath
                (string-append dir "/example.nar")  ; URL
                (%current-system)                   ; System
                (basename
                 (derivation-file-name d)))))       ; Deriver

    ;; Remove entry from the local cache.
    (false-if-exception
     (delete-file (string-append (getenv "XDG_CACHE_HOME")
                                 "/guix/substitute-binary/"
                                 (store-path-hash-part o))))

    ;; Make sure `substitute-binary' correctly communicates the above data.
    (set-build-options s #:use-substitutes? #t)
    (and (has-substitutes? s o)
         (equal? (list o) (substitutable-paths s (list o)))
         (match (pk 'spi (substitutable-path-info s (list o)))
           (((? substitutable? s))
            (and (string=? (substitutable-deriver s) (derivation-file-name d))
                 (null? (substitutable-references s))
                 (equal? (substitutable-nar-size s) 1234)))))))

(test-assert "substitute"
  (let* ((s   (open-connection))
         (c   (random-text))                      ; contents of the output
         (d   (build-expression->derivation
               s "substitute-me" (%current-system)
               `(call-with-output-file %output
                  (lambda (p)
                    (exit 1)                      ; would actually fail
                    (display ,c p)))
               '()
               #:guile-for-build
               (package-derivation s %bootstrap-guile (%current-system))))
         (o   (derivation->output-path d))
         (dir (and=> (getenv "GUIX_BINARY_SUBSTITUTE_URL")
                     (compose uri-path string->uri))))
    ;; Create fake substituter data, to be read by `substitute-binary'.
    (call-with-output-file (string-append dir "/nix-cache-info")
      (lambda (p)
        (format p "StoreDir: ~a\nWantMassQuery: 0\n"
                (%store-prefix))))
    (call-with-output-file (string-append dir "/example.out")
      (lambda (p)
        (display c p)))
    (call-with-output-file (string-append dir "/example.nar")
      (lambda (p)
        (write-file (string-append dir "/example.out") p)))
    (call-with-output-file (string-append dir "/" (store-path-hash-part o)
                                          ".narinfo")
      (lambda (p)
        (format p "StorePath: ~a
URL: ~a
Compression: none
NarSize: 1234
NarHash: sha256:~a
References: 
System: ~a
Deriver: ~a~%"
                o                                   ; StorePath
                "example.nar"                       ; relative URL
                (call-with-input-file (string-append dir "/example.nar")
                  (compose bytevector->nix-base32-string sha256
                           get-bytevector-all))
                (%current-system)                   ; System
                (basename
                 (derivation-file-name d)))))       ; Deriver

    ;; Make sure we use `substitute-binary'.
    (set-build-options s #:use-substitutes? #t)
    (and (has-substitutes? s o)
         (build-derivations s (list d))
         (equal? c (call-with-input-file o get-string-all)))))

(test-assert "substitute --fallback"
  (let* ((s   (open-connection))
         (t   (random-text))                      ; contents of the output
         (d   (build-expression->derivation
               s "substitute-me-not" (%current-system)
               `(call-with-output-file %output
                  (lambda (p)
                    (display ,t p)))
               '()
               #:guile-for-build
               (package-derivation s %bootstrap-guile (%current-system))))
         (o   (derivation->output-path d))
         (dir (and=> (getenv "GUIX_BINARY_SUBSTITUTE_URL")
                     (compose uri-path string->uri))))
    ;; Create fake substituter data, to be read by `substitute-binary'.
    (call-with-output-file (string-append dir "/nix-cache-info")
      (lambda (p)
        (format p "StoreDir: ~a\nWantMassQuery: 0\n"
                (%store-prefix))))
    (call-with-output-file (string-append dir "/" (store-path-hash-part o)
                                          ".narinfo")
      (lambda (p)
        (format p "StorePath: ~a
URL: ~a
Compression: none
NarSize: 1234
NarHash: sha256:0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
References: 
System: ~a
Deriver: ~a~%"
                o                                   ; StorePath
                "does-not-exist.nar"                ; relative URL
                (%current-system)                   ; System
                (basename
                 (derivation-file-name d)))))       ; Deriver

    ;; Make sure we use `substitute-binary'.
    (set-build-options s #:use-substitutes? #t)
    (and (has-substitutes? s o)
         (guard (c ((nix-protocol-error? c)
                    ;; The substituter failed as expected.  Now make sure that
                    ;; #:fallback? #t works correctly.
                    (set-build-options s
                                       #:use-substitutes? #t
                                       #:fallback? #t)
                    (and (build-derivations s (list d))
                         (equal? t (call-with-input-file o get-string-all)))))
           ;; Should fail.
           (build-derivations s (list d))
           #f))))

(test-end "store")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
