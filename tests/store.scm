;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix serialization)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;; Test the (guix store) module.

(define %store
  (open-connection-for-tests))


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

(test-equal "store-path-package-name"
  "guile-2.0.7"
  (store-path-package-name
   (string-append (%store-prefix)
                  "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7")))

(test-equal "store-path-package-name #f"
  #f
  (store-path-package-name
   "/foo/bar/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7"))

(test-assert "direct-store-path?"
  (and (direct-store-path?
        (string-append (%store-prefix)
                       "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7"))
       (not (direct-store-path?
             (string-append
              (%store-prefix)
              "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7/bin/guile")))
       (not (direct-store-path? (%store-prefix)))))

(test-skip (if %store 0 13))

(test-assert "valid-path? live"
  (let ((p (add-text-to-store %store "hello" "hello, world")))
    (valid-path? %store p)))

(test-assert "valid-path? false"
  (not (valid-path? %store
                    (string-append (%store-prefix) "/"
                                   (make-string 32 #\e) "-foobar"))))

(test-assert "valid-path? error"
  (with-store s
    (guard (c ((nix-protocol-error? c) #t))
      (valid-path? s "foo")
      #f)))

(test-assert "valid-path? recovery"
  ;; Prior to Nix commit 51800e0 (18 Mar. 2014), the daemon would immediately
  ;; close the connection after receiving a 'valid-path?' RPC with a non-store
  ;; file name.  See
  ;; <http://article.gmane.org/gmane.linux.distributions.nixos/12411> for
  ;; details.
  (with-store s
    (let-syntax ((true-if-error (syntax-rules ()
                                  ((_ exp)
                                   (guard (c ((nix-protocol-error? c) #t))
                                     exp #f)))))
      (and (true-if-error (valid-path? s "foo"))
           (true-if-error (valid-path? s "bar"))
           (true-if-error (valid-path? s "baz"))
           (true-if-error (valid-path? s "chbouib"))
           (valid-path? s (add-text-to-store s "valid" "yeah"))))))

(test-assert "hash-part->path"
  (let ((p (add-text-to-store %store "hello" "hello, world")))
    (equal? (hash-part->path %store (store-path-hash-part p))
            p)))

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

(test-assert "permanent root"
  (let* ((p  (with-store store
               (let ((p (add-text-to-store store "random-text"
                                           (random-text))))
                 (add-permanent-root p)
                 (add-permanent-root p)           ; should not throw
                 p))))
    (and (member p (live-paths %store))
         (begin
           (remove-permanent-root p)
           (->bool (member p (dead-paths %store)))))))

(test-assert "dead path can be explicitly collected"
  (let ((p (add-text-to-store %store "random-text"
                              (random-text) '())))
    (let-values (((paths freed) (delete-paths %store (list p))))
      (and (equal? paths (list p))
           (> freed 0)
           (not (file-exists? p))))))

(test-assert "add-text-to-store vs. delete-paths"
  ;; Before, 'add-text-to-store' would return PATH2 without noticing that it
  ;; is no longer valid.
  (with-store store
    (let* ((text    (random-text))
           (path    (add-text-to-store store "delete-me" text))
           (deleted (delete-paths store (list path)))
           (path2   (add-text-to-store store "delete-me" text)))
      (and (string=? path path2)
           (equal? deleted (list path))
           (valid-path? store path)
           (file-exists? path)))))

(test-assert "add-to-store vs. delete-paths"
  ;; Same as above.
  (with-store store
    (let* ((file    (search-path %load-path "guix.scm"))
           (path    (add-to-store store "delete-me" #t "sha256" file))
           (deleted (delete-paths store (list path)))
           (path2   (add-to-store store "delete-me" #t "sha256" file)))
      (and (string=? path path2)
           (equal? deleted (list path))
           (valid-path? store path)
           (file-exists? path)))))

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

(test-assert "topologically-sorted, one item"
  (let* ((a (add-text-to-store %store "a" "a"))
         (b (add-text-to-store %store "b" "b" (list a)))
         (c (add-text-to-store %store "c" "c" (list b)))
         (d (add-text-to-store %store "d" "d" (list c)))
         (s (topologically-sorted %store (list d))))
    (equal? s (list a b c d))))

(test-assert "topologically-sorted, several items"
  (let* ((a  (add-text-to-store %store "a" "a"))
         (b  (add-text-to-store %store "b" "b" (list a)))
         (c  (add-text-to-store %store "c" "c" (list b)))
         (d  (add-text-to-store %store "d" "d" (list c)))
         (s1 (topologically-sorted %store (list d a c b)))
         (s2 (topologically-sorted %store (list b d c a b d))))
    (equal? s1 s2 (list a b c d))))

(test-assert "topologically-sorted, more difficult"
  (let* ((a  (add-text-to-store %store "a" "a"))
         (b  (add-text-to-store %store "b" "b" (list a)))
         (c  (add-text-to-store %store "c" "c" (list b)))
         (d  (add-text-to-store %store "d" "d" (list c)))
         (w  (add-text-to-store %store "w" "w"))
         (x  (add-text-to-store %store "x" "x" (list w)))
         (y  (add-text-to-store %store "y" "y" (list x d)))
         (s1 (topologically-sorted %store (list y)))
         (s2 (topologically-sorted %store (list c y)))
         (s3 (topologically-sorted %store (cons y (references %store y)))))
    ;; The order in which 'references' returns the references of Y is
    ;; unspecified, so accommodate.
    (let* ((x-then-d? (equal? (references %store y) (list x d))))
      (and (equal? s1
                   (if x-then-d?
                       (list w x a b c d y)
                       (list a b c d w x y)))
           (equal? s2
                   (if x-then-d?
                       (list a b c w x d y)
                       (list a b c d w x y)))
           (lset= string=? s1 s3)))))

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
               s "substitute-me"
               `(call-with-output-file %output
                  (lambda (p)
                    (exit 1)                      ; would actually fail
                    (display ,c p)))
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

(test-assert "substitute, corrupt output hash"
  ;; Tweak the substituter into installing a substitute whose hash doesn't
  ;; match the one announced in the narinfo.  The daemon must notice this and
  ;; raise an error.
  (let* ((s   (open-connection))
         (c   "hello, world")                     ; contents of the output
         (d   (build-expression->derivation
               s "corrupt-substitute"
               `(mkdir %output)
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
        (display "The contents here do not match C." p)))
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
                (bytevector->nix-base32-string
                 (sha256 (string->utf8 c)))
                (%current-system)                   ; System
                (basename
                 (derivation-file-name d)))))       ; Deriver

    ;; Make sure we use `substitute-binary'.
    (set-build-options s
                       #:use-substitutes? #t
                       #:fallback? #f)
    (and (has-substitutes? s o)
         (guard (c ((nix-protocol-error? c)
                    ;; XXX: the daemon writes "hash mismatch in downloaded
                    ;; path", but the actual error returned to the client
                    ;; doesn't mention that.
                    (pk 'corrupt c)
                    (not (zero? (nix-protocol-error-status c)))))
           (build-derivations s (list d))
           #f))))

(test-assert "substitute --fallback"
  (let* ((s   (open-connection))
         (t   (random-text))                      ; contents of the output
         (d   (build-expression->derivation
               s "substitute-me-not"
               `(call-with-output-file %output
                  (lambda (p)
                    (display ,t p)))
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

(test-assert "export/import several paths"
  (let* ((texts (unfold (cut >= <> 10)
                        (lambda _ (random-text))
                        1+
                        0))
         (files (map (cut add-text-to-store %store "text" <>) texts))
         (dump  (call-with-bytevector-output-port
                 (cut export-paths %store files <>))))
    (delete-paths %store files)
    (and (every (negate file-exists?) files)
         (let* ((source   (open-bytevector-input-port dump))
                (imported (import-paths %store source)))
           (and (equal? imported files)
                (every file-exists? files)
                (equal? texts
                        (map (lambda (file)
                               (call-with-input-file file
                                 get-string-all))
                             files)))))))

(test-assert "export/import paths, ensure topological order"
  (let* ((file0 (add-text-to-store %store "baz" (random-text)))
         (file1 (add-text-to-store %store "foo" (random-text)
                                   (list file0)))
         (file2 (add-text-to-store %store "bar" (random-text)
                                   (list file1)))
         (files (list file1 file2))
         (dump1 (call-with-bytevector-output-port
                 (cute export-paths %store (list file1 file2) <>)))
         (dump2 (call-with-bytevector-output-port
                 (cute export-paths %store (list file2 file1) <>))))
    (delete-paths %store files)
    (and (every (negate file-exists?) files)
         (bytevector=? dump1 dump2)
         (let* ((source   (open-bytevector-input-port dump1))
                (imported (import-paths %store source)))
           ;; DUMP1 should contain exactly FILE1 and FILE2, not FILE0.
           (and (equal? imported (list file1 file2))
                (every file-exists? files)
                (equal? (list file0) (references %store file1))
                (equal? (list file1) (references %store file2)))))))

(test-assert "import corrupt path"
  (let* ((text (random-text))
         (file (add-text-to-store %store "text" text))
         (dump (call-with-bytevector-output-port
                (cut export-paths %store (list file) <>))))
    (delete-paths %store (list file))

    ;; Flip a bit in the stream's payload.
    (let* ((index (quotient (bytevector-length dump) 4))
           (byte  (bytevector-u8-ref dump index)))
      (bytevector-u8-set! dump index (logxor #xff byte)))

    (and (not (file-exists? file))
         (guard (c ((nix-protocol-error? c)
                    (pk 'c c)
                    (and (not (zero? (nix-protocol-error-status c)))
                         (string-contains (nix-protocol-error-message c)
                                          "corrupt"))))
           (let* ((source   (open-bytevector-input-port dump))
                  (imported (import-paths %store source)))
             (pk 'corrupt-imported imported)
             #f)))))

(test-assert "register-path"
  (let ((file (string-append (%store-prefix) "/" (make-string 32 #\f)
                             "-fake")))
    (when (valid-path? %store file)
      (delete-paths %store (list file)))
    (false-if-exception (delete-file file))

    (let ((ref (add-text-to-store %store "ref-of-fake" (random-text)))
          (drv (string-append file ".drv")))
      (call-with-output-file file
        (cut display "This is a fake store item.\n" <>))
      (register-path file
                     #:references (list ref)
                     #:deriver drv)

      (and (valid-path? %store file)
           (equal? (references %store file) (list ref))
           (null? (valid-derivers %store file))
           (null? (referrers %store file))))))

(test-equal "store-lower"
  "Lowered."
  (let* ((add  (store-lower text-file))
         (file (add %store "foo" "Lowered.")))
    (call-with-input-file file get-string-all)))

(test-end "store")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
