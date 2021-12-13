;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module ((gcrypt hash) #:prefix gcrypt:)
  #:use-module ((gcrypt pk-crypto) #:prefix gcrypt:)
  #:use-module (guix pki)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix serialization)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
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

(define %shell
  (or (getenv "SHELL") (getenv "CONFIG_SHELL") "/bin/sh"))


(test-begin "store")

(test-assert "open-connection with file:// URI"
  (let ((store (open-connection (string-append "file://"
                                               (%daemon-socket-uri)))))
    (and (add-text-to-store store "foo" "bar")
         (begin
           (close-connection store)
           #t))))

(test-equal "connection handshake error"
  EPROTO
  (let ((port (%make-void-port "rw")))
    (guard (c ((store-connection-error? c)
               (and (eq? port (store-connection-error-file c))
                    (store-connection-error-code c))))
      (open-connection #f #:port port)
      'broken)))

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

(test-skip (if %store 0 15))

(test-equal "profiles/per-user exists and is not writable"
  #o755
  (stat:perms (stat (string-append %state-directory "/profiles/per-user"))))

(test-equal "profiles/per-user/$USER exists"
  (list (getuid) #o755)
  (let ((s (stat (string-append %state-directory "/profiles/per-user/"
                                (passwd:name (getpwuid (getuid)))))))
    (list (stat:uid s) (stat:perms s))))

(test-equal "add-to-store"
  '("sha1" "sha256" "sha512" "sha3-256" "sha3-512" "blake2s-256")
  (let* ((file    (search-path %load-path "guix.scm"))
         (content (call-with-input-file file get-bytevector-all)))
    (map (lambda (hash-algo)
           (let ((file (add-to-store %store "guix.scm" #f hash-algo file)))
             (and (direct-store-path? file)
                  (bytevector=? (call-with-input-file file get-bytevector-all)
                                content)
                  hash-algo)))
         '("sha1" "sha256" "sha512" "sha3-256" "sha3-512" "blake2s-256"))))

(test-equal "add-data-to-store"
  #vu8(1 2 3 4 5)
  (call-with-input-file (add-data-to-store %store "data" #vu8(1 2 3 4 5))
    get-bytevector-all))

(test-assert "valid-path? live"
  (let ((p (add-text-to-store %store "hello" "hello, world")))
    (valid-path? %store p)))

(test-assert "valid-path? false"
  (not (valid-path? %store
                    (string-append (%store-prefix) "/"
                                   (make-string 32 #\e) "-foobar"))))

(test-equal "with-store, multiple values"        ;<https://bugs.gnu.org/42912>
  '(1 2 3)
  (call-with-values
      (lambda ()
        (with-store s
          (add-text-to-store s "foo" "bar")
          (values 1 2 3)))
    list))

(test-assert "valid-path? error"
  (with-store s
    (guard (c ((store-protocol-error? c) #t))
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
                                   (guard (c ((store-protocol-error? c) #t))
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
    (->bool (member p (dead-paths %store)))))

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

(test-assert "add-indirect-root and find-roots"
  (call-with-temporary-directory
   (lambda (directory)
     (let* ((item (add-text-to-store %store "something" (random-text)))
            (root (string-append directory "/gc-root")))
       (symlink item root)
       (add-indirect-root %store root)
       (let ((result (member (cons root item) (find-roots %store))))
         (delete-file root)
         result)))))

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
           ;; XXX: On some file systems (notably Btrfs), freed
           ;; may return 0.  See <https://bugs.gnu.org/29363>.
           ;;(> freed 0)
           (not (file-exists? p))))))

(test-assert "add-text-to-store/add-to-store vs. delete-paths"
  ;; Before, 'add-text-to-store' and 'add-to-store' would return the same
  ;; store item without noticing that it is no longer valid.
  (with-store store
    (let* ((text    (random-text))
           (file    (search-path %load-path "guix.scm"))
           (path1   (add-text-to-store store "delete-me" text))
           (path2   (add-to-store store "delete-me" #t "sha256" file))
           (deleted (delete-paths store (list path1 path2))))
      (and (string=? path1 (add-text-to-store store "delete-me" text))
           (string=? path2 (add-to-store store "delete-me" #t "sha256" file))
           (lset= string=? deleted (list path1 path2))
           (valid-path? store path1)
           (valid-path? store path2)
           (file-exists? path1)
           (file-exists? path2)))))

(test-equal "add-file-tree-to-store"
  `(42
    ("." directory #t)
    ("./bar" directory #t)
    ("./foo" directory #t)
    ("./foo/a" regular "file a")
    ("./foo/b" symlink "a")
    ("./foo/c" directory #t)
    ("./foo/c/p" regular "file p")
    ("./foo/c/q" directory #t)
    ("./foo/c/q/x" regular
     ,(string-append "#!" %shell "\nexit 42"))
    ("./foo/c/q/y" symlink "..")
    ("./foo/c/q/z" directory #t))
  (let* ((tree  `("file-tree" directory
                  ("foo" directory
                   ("a" regular (data "file a"))
                   ("b" symlink "a")
                   ("c" directory
                    ("p" regular (data ,(string->utf8 "file p")))
                    ("q" directory
                     ("x" executable
                      (data ,(string-append "#!" %shell "\nexit 42")))
                     ("y" symlink "..")
                     ("z" directory))))
                  ("bar" directory)))
         (result (add-file-tree-to-store %store tree)))
    (cons (status:exit-val (system* (string-append result "/foo/c/q/x")))
          (with-directory-excursion result
            (map (lambda (file)
                   (let ((type (stat:type (lstat file))))
                     `(,file ,type
                             ,(match type
                                ((or 'regular 'executable)
                                 (call-with-input-file file
                                   get-string-all))
                                ('symlink (readlink file))
                                ('directory #t)))))
                 (find-files "." #:directories? #t))))))

(test-equal "add-file-tree-to-store, flat"
  "Hello, world!"
  (let* ((tree   `("flat-file" regular (data "Hello, world!")))
         (result (add-file-tree-to-store %store tree)))
    (and (file-exists? result)
         (call-with-input-file result get-string-all))))

(test-assert "references"
  (let* ((t1 (add-text-to-store %store "random1"
                                (random-text)))
         (t2 (add-text-to-store %store "random2"
                                (random-text) (list t1))))
    (and (equal? (list t1) (references %store t2))
         (equal? (list t2) (referrers %store t1))
         (null? (references %store t1))
         (null? (referrers %store t2)))))

(test-equal "substitutable-path-info when substitutes are turned off"
  '()
  (with-store s
    (set-build-options s #:use-substitutes? #f)
    (let* ((b  (add-to-store s "bash" #t "sha256"
                             (search-bootstrap-binary "bash"
                                                      (%current-system))))
           (d  (derivation s "the-thing" b '("--version")
                           #:inputs `((,b))))
           (o  (derivation->output-path d)))
      (with-derivation-narinfo d
        (substitutable-path-info s (list o))))))

(test-equal "substitutable-paths when substitutes are turned off"
  '()
  (with-store s
    (set-build-options s #:use-substitutes? #f)
    (let* ((b  (add-to-store s "bash" #t "sha256"
                             (search-bootstrap-binary "bash"
                                                      (%current-system))))
           (d  (derivation s "the-thing" b '("--version")
                           #:inputs `((,b))))
           (o  (derivation->output-path d)))
      (with-derivation-narinfo d
        (substitutable-paths s (list o))))))

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

    (and (same? (requisites %store (list t1)) (list t1))
         (same? (requisites %store (list t2)) (list t1 t2))
         (same? (requisites %store (list t3)) (list t1 t2 t3))
         (same? (requisites %store (list t4)) (list t1 t2 t3 t4))
         (same? (requisites %store (list t1 t2 t3 t4))
                (list t1 t2 t3 t4)))))

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

(test-equal "with-build-handler"
  'success
  (let* ((b  (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s  (add-to-store %store "bash" #t "sha256"
                           (search-bootstrap-binary "bash"
                                                    (%current-system))))
         (d1 (derivation %store "the-thing"
                         s `("-e" ,b)
                         #:env-vars `(("foo" . ,(random-text)))
                         #:sources (list b s)))
         (d2 (derivation %store "the-thing"
                         s `("-e" ,b)
                         #:env-vars `(("foo" . ,(random-text))
                                      ("bar" . "baz"))
                         #:sources (list b s)))
         (o1 (derivation->output-path d1))
         (o2 (derivation->output-path d2)))
    (with-build-handler
        (let ((counter 0))
          (lambda (continue store things mode)
            (match things
              ((drv)
               (set! counter (+ 1 counter))
               (if (string=? drv (derivation-file-name d1))
                   (continue #t)
                   (and (string=? drv (derivation-file-name d2))
                        (= counter 2)
                        'success))))))
      (build-derivations %store (list d1))
      (build-derivations %store (list d2))
      'fail)))

(test-equal "with-build-handler + with-store"
  'success
  ;; Check that STORE remains valid when the build handler invokes CONTINUE,
  ;; even though 'with-build-handler' is outside the dynamic extent of
  ;; 'with-store'.
  (with-build-handler (lambda (continue store things mode)
                        (match things
                          ((drv)
                           (and (string-suffix? "thingie.drv" drv)
                                (not (port-closed?
                                      (store-connection-socket store)))
                                (continue #t)))))
    (with-store store
      (let* ((b (add-text-to-store store "build" "echo $foo > $out" '()))
             (s (add-to-store store "bash" #t "sha256"
                              (search-bootstrap-binary "bash"
                                                       (%current-system))))
             (d (derivation store "thingie"
                            s `("-e" ,b)
                            #:env-vars `(("foo" . ,(random-text)))
                            #:sources (list b s))))
        (build-derivations store (list d))

        ;; Here STORE's socket should still be open.
        (and (valid-path? store (derivation->output-path d))
             'success)))))

(test-assert "map/accumulate-builds"
  (let* ((b  (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s  (add-to-store %store "bash" #t "sha256"
                           (search-bootstrap-binary "bash"
                                                    (%current-system))))
         (d1 (derivation %store "the-thing"
                         s `("-e" ,b)
                         #:env-vars `(("foo" . ,(random-text)))
                         #:sources (list b s)))
         (d2 (derivation %store "the-thing"
                         s `("-e" ,b)
                         #:env-vars `(("foo" . ,(random-text))
                                      ("bar" . "baz"))
                         #:sources (list b s))))
    (with-build-handler (lambda (continue store things mode)
                          (equal? (map derivation-file-name (list d1 d2))
                                  things))
      (map/accumulate-builds %store
                             (lambda (drv)
                               (build-derivations %store (list drv))
                               (add-to-store %store "content-addressed"
                                             #t "sha256"
                                             (derivation->output-path drv)))
                             (list d1 d2)))))

(test-equal "map/accumulate-builds cutoff" ;https://issues.guix.gnu.org/50264
  (iota 20)

  ;; Make sure that, when the cutoff is reached, 'map/accumulate-builds' still
  ;; returns the right result and calls the build handler by batches.
  (let* ((b  (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s  (add-to-store %store "bash" #t "sha256"
                           (search-bootstrap-binary "bash"
                                                    (%current-system))))
         (d  (map (lambda (i)
                    (derivation %store (string-append "the-thing-"
                                                      (number->string i))
                                s `("-e" ,b)
                                #:env-vars `(("foo" . ,(random-text)))
                                #:sources (list b s)
                                #:properties `((n . ,i))))
                  (iota 20)))
         (calls '()))
    (define lst
      (with-build-handler (lambda (continue store things mode)
                            (set! calls (cons things calls))
                            (continue #f))
        (map/accumulate-builds %store
                               (lambda (d)
                                 (build-derivations %store (list d))
                                 (assq-ref (derivation-properties d) 'n))
                               d
                               #:cutoff 7)))

    (match (reverse calls)
      (((batch1 ...) (batch2 ...) (batch3 ...))
       (and (equal? (map derivation-file-name (take d 8)) batch1)
            (equal? (map derivation-file-name (take (drop d 8) 8)) batch2)
            (equal? (map derivation-file-name (drop d 16)) batch3)
            lst)))))

(test-equal "map/accumulate-builds and different store"
  '(d2)                               ;see <https://issues.guix.gnu.org/46756>
  (let* ((b  (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s  (add-to-store %store "bash" #t "sha256"
                           (search-bootstrap-binary "bash"
                                                    (%current-system))))
         (d1 (derivation %store "first"
                         s `("-e" ,b)
                         #:env-vars `(("foo" . ,(random-text)))
                         #:sources (list b s)))
         (d2 (derivation %store "second"
                         s `("-e" ,b)
                         #:env-vars `(("foo" . ,(random-text))
                                      ("bar" . "baz"))
                         #:sources (list b s))))
    (with-store alternate-store
      (with-build-handler (lambda (continue store things mode)
                            ;; If this handler is called, it means that
                            ;; 'map/accumulate-builds' triggered a build,
                            ;; which it shouldn't since the inner
                            ;; 'build-derivations' call is for another store.
                            'failed)
        (map/accumulate-builds %store
                               (lambda (drv)
                                 (build-derivations alternate-store (list d2))
                                 'd2)
                               (list d1))))))

(test-assert "mapm/accumulate-builds"
  (let* ((d1 (run-with-store %store
               (gexp->derivation "foo" #~(mkdir #$output))))
         (d2 (run-with-store %store
               (gexp->derivation "bar" #~(mkdir #$output)))))
    (with-build-handler (lambda (continue store things mode)
                          (equal? (map derivation-file-name (pk 'zz (list d1 d2)))
                                  (pk 'XX things)))
      (run-with-store %store
        (mapm/accumulate-builds built-derivations `((,d1) (,d2)))))))

(test-equal "mapm/accumulate-builds, %current-target-system"
  (make-list 2 '("i586-pc-gnu" "i586-pc-gnu"))
  ;; Both the 'mapm' and 'mapm/accumulate-builds' procedures should see the
  ;; right #:target.
  (run-with-store %store
    (mlet %store-monad ((lst1 (mapm %store-monad
                                    (lambda _
                                      (current-target-system))
                                    '(a b)))
                        (lst2 (mapm/accumulate-builds
                               (lambda _
                                 (current-target-system))
                               '(a b))))
      (return (list lst1 lst2)))
    #:system system
    #:target "i586-pc-gnu"))

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

(test-assert "current-build-output-port, UTF-8"
  ;; Are UTF-8 strings in the build log properly interpreted?
  (string-contains
   (with-fluids ((%default-port-encoding "UTF-8")) ;for the string port
     (call-with-output-string
      (lambda (port)
        (parameterize ((current-build-output-port port))
          (let* ((s "Here’s a Greek letter: λ.")
                 (d (build-expression->derivation
                     %store "foo" `(display ,s)
                     #:guile-for-build
                     (package-derivation %store %bootstrap-guile
                                         (%current-system)))))
            (guard (c ((store-protocol-error? c) #t))
              (build-derivations %store (list d))))))))
   "Here’s a Greek letter: λ."))

(test-assert "current-build-output-port, UTF-8 + garbage"
  ;; What about a mixture of UTF-8 + garbage?
  (string-contains
   (with-fluids ((%default-port-encoding "UTF-8")) ;for the string port
     (call-with-output-string
      (lambda (port)
        (parameterize ((current-build-output-port port))
          (let ((d (build-expression->derivation
                    %store "foo"
                    `(begin
                       (use-modules (rnrs io ports))
                       (display "garbage: ")
                       (put-bytevector (current-output-port) #vu8(128))
                       (display "lambda: λ\n"))
                     #:guile-for-build
                     (package-derivation %store %bootstrap-guile))))
            (guard (c ((store-protocol-error? c) #t))
              (build-derivations %store (list d))))))))
   "garbage: �lambda: λ"))

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
  (with-store s
    (let* ((d1 (package-derivation s %bootstrap-guile (%current-system)))
           (d2 (package-derivation s %bootstrap-glibc (%current-system)))
           (o  (map derivation->output-path (list d1 d2))))
      (set-build-options s #:use-substitutes? #f)
      (and (not (has-substitutes? s (derivation-file-name d1)))
           (not (has-substitutes? s (derivation-file-name d2)))
           (null? (substitutable-paths s o))
           (null? (substitutable-path-info s o))))))

(test-assert "build-things with output path"
  (with-store s
    (let* ((c   (random-text))                    ;contents of the output
           (d   (build-expression->derivation
                 s "substitute-me"
                 `(call-with-output-file %output
                    (lambda (p)
                      (display ,c p)))
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (set-build-options s #:use-substitutes? #f)

      ;; Pass 'build-things' the output file name, O.  However, since there
      ;; are no substitutes for O, it will just do nothing.
      (build-things s (list o))
      (not (valid-path? s o)))))

(test-skip (if (getenv "GUIX_BINARY_SUBSTITUTE_URL") 0 1))

(test-assert "substitute query"
  (with-store s
    (let* ((d (package-derivation s %bootstrap-guile (%current-system)))
           (o (derivation->output-path d)))
      ;; Create fake substituter data, to be read by 'guix substitute'.
      (with-derivation-narinfo d
        ;; Remove entry from the local cache.
        (false-if-exception
         (delete-file-recursively (string-append (getenv "XDG_CACHE_HOME")
                                                 "/guix/substitute")))

        ;; Make sure 'guix substitute' correctly communicates the above
        ;; data.
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (equal? (list o) (substitutable-paths s (list o)))
             (match (pk 'spi (substitutable-path-info s (list o)))
               (((? substitutable? s))
                (and (string=? (substitutable-deriver s)
                               (derivation-file-name d))
                     (null? (substitutable-references s))
                     (equal? (substitutable-nar-size s) 1234)))))))))

(test-assert "substitute query, alternating URLs"
  (let* ((d (with-store s
              (package-derivation s %bootstrap-guile (%current-system))))
         (o (derivation->output-path d)))
    (with-derivation-narinfo d
      ;; Remove entry from the local cache.
      (false-if-exception
       (delete-file-recursively (string-append (getenv "XDG_CACHE_HOME")
                                               "/guix/substitute")))

      ;; Note: We reconnect to the daemon to force a new instance of 'guix
      ;; substitute' to be used; otherwise the #:substitute-urls of
      ;; 'set-build-options' would have no effect.

      (and (with-store s                        ;the right substitute URL
             (set-build-options s #:use-substitutes? #t
                                #:substitute-urls (%test-substitute-urls))
             (has-substitutes? s o))
           (with-store s                        ;the wrong one
             (set-build-options s #:use-substitutes? #t
                                #:substitute-urls (list
                                                   "http://does-not-exist"))
             (not (has-substitutes? s o)))
           (with-store s                        ;the right one again
             (set-build-options s #:use-substitutes? #t
                                #:substitute-urls (%test-substitute-urls))
             (has-substitutes? s o))
           (with-store s                        ;empty list of URLs
             (set-build-options s #:use-substitutes? #t
                                #:substitute-urls '())
             (not (has-substitutes? s o)))))))

(test-assert "substitute"
  (with-store s
    (let* ((c   (random-text))                     ; contents of the output
           (d   (build-expression->derivation
                 s "substitute-me"
                 `(call-with-output-file %output
                    (lambda (p)
                      (exit 1)                     ; would actually fail
                      (display ,c p)))
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (with-derivation-substitute d c
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (build-derivations s (list d))
             (canonical-file? o)
             (equal? c (call-with-input-file o get-string-all)))))))

(test-assert "substitute, deduplication"
  (with-store s
    ;; Note: C must be longer than %DEDUPLICATION-MINIMUM-SIZE.
    (let* ((c   (string-concatenate
                 (make-list 200 (random-text))))  ; contents of the output
           (g   (package-derivation s %bootstrap-guile))
           (d1  (build-expression->derivation s "substitute-me"
                                              `(begin ,c (exit 1))
                                              #:guile-for-build g))
           (d2  (build-expression->derivation s "build-me"
                                              `(call-with-output-file %output
                                                 (lambda (p)
                                                   (display ,c p)))
                                              #:guile-for-build g))
           (o1  (derivation->output-path d1))
           (o2  (derivation->output-path d2)))
      (with-derivation-substitute d1 c
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o1)
             (build-derivations s (list d2))      ;build
             (build-derivations s (list d1))      ;substitute
             (canonical-file? o1)
             (equal? c (call-with-input-file o1 get-string-all))
             (= (stat:ino (stat o1)) (stat:ino (stat o2))))))))

(test-assert "substitute + build-things with output path"
  (with-store s
    (let* ((c   (random-text))                    ;contents of the output
           (d   (build-expression->derivation
                 s "substitute-me"
                 `(call-with-output-file %output
                    (lambda (p)
                      (exit 1)                    ;would actually fail
                      (display ,c p)))
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (with-derivation-substitute d c
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (build-things s (list o))            ;give the output path
             (valid-path? s o)
             (canonical-file? o)
             (equal? c (call-with-input-file o get-string-all)))))))

(test-assert "substitute + build-things with specific output"
  (with-store s
    (let* ((c   (random-text))                    ;contents of the output
           (d   (build-expression->derivation
                 s "substitute-me" `(begin ,c (exit 1)) ;would fail
                 #:outputs '("out" "one" "two")
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (with-derivation-substitute d c
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)

             ;; Ask for nothing but the "out" output of D.
             (build-things s `((,(derivation-file-name d) . "out")))

             (valid-path? s o)
             (canonical-file? o)
             (equal? c (call-with-input-file o get-string-all)))))))

(test-assert "substitute, corrupt output hash"
  ;; Tweak the substituter into installing a substitute whose hash doesn't
  ;; match the one announced in the narinfo.  The daemon must notice this and
  ;; raise an error.
  (with-store s
    (let* ((c   "hello, world")                    ; contents of the output
           (d   (build-expression->derivation
                 s "corrupt-substitute"
                 `(mkdir %output)
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (with-derivation-substitute d c
        (sha256 => (make-bytevector 32 0)) ;select a hash that doesn't match C

        ;; Make sure we use 'guix substitute'.
        (set-build-options s
                           #:use-substitutes? #t
                           #:fallback? #f
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (guard (c ((store-protocol-error? c)
                        ;; XXX: the daemon writes "hash mismatch in downloaded
                        ;; path", but the actual error returned to the client
                        ;; doesn't mention that.
                        (pk 'corrupt c)
                        (not (zero? (store-protocol-error-status c)))))
               (build-derivations s (list d))
               #f))))))

(test-assert "substitute, corrupt output hash, build trace"
  ;; Likewise, and check the build trace.
  (with-store s
    (let* ((c   "hello, world")                   ; contents of the output
           (d   (build-expression->derivation
                 s "corrupt-substitute"
                 `(mkdir %output)
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      ;; Make sure we use 'guix substitute'.
      (set-build-options s
                         #:print-build-trace #t
                         #:use-substitutes? #t
                         #:fallback? #f
                         #:substitute-urls (%test-substitute-urls))

      (with-derivation-substitute d c
        (sha256 => (make-bytevector 32 0)) ;select a hash that doesn't match C

        (define output
          (call-with-output-string
            (lambda (port)
              (parameterize ((current-build-output-port port))
                (guard (c ((store-protocol-error? c) #t))
                  (build-derivations s (list d))
                  #f)))))

        (define actual-hash
          (let-values (((port get-hash)
                        (gcrypt:open-hash-port
                         (gcrypt:hash-algorithm gcrypt:sha256))))
            (write-file-tree "foo" port
                             #:file-type+size
                             (lambda _
                               (values 'regular (string-length c)))
                             #:file-port
                             (lambda _
                               (open-input-string c)))
            (close-port port)
            (bytevector->nix-base32-string (get-hash))))

        (define expected-hash
          (bytevector->nix-base32-string (make-bytevector 32 0)))

        (define mismatch
          (string-append "@ hash-mismatch " o " sha256 "
                         expected-hash " " actual-hash "\n"))

        (define failure
          (string-append "@ substituter-failed " o))

        (and (string-contains output mismatch)
             (string-contains output failure))))))

(test-assert "substitute --fallback"
  (with-store s
    (let* ((t   (random-text))                    ; contents of the output
           (d   (build-expression->derivation
                 s "substitute-me-not"
                 `(call-with-output-file %output
                    (lambda (p)
                      (display ,t p)))
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      ;; Create fake substituter data, to be read by 'guix substitute'.
      (with-derivation-narinfo d
        ;; Make sure we use 'guix substitute'.
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (guard (c ((store-protocol-error? c)
                        ;; The substituter failed as expected.  Now make
                        ;; sure that #:fallback? #t works correctly.
                        (set-build-options s
                                           #:use-substitutes? #t
                                           #:substitute-urls
                                             (%test-substitute-urls)
                                           #:fallback? #t)
                        (and (build-derivations s (list d))
                             (equal? t (call-with-input-file o
                                         get-string-all)))))
               ;; Should fail.
               (build-derivations s (list d))
               #f))))))

(test-equal "substitute query and large size"
  (+ 100 (expt 2 63))                     ;<https://issues.guix.gnu.org/51983>
  (with-store s
    (let* ((size (+ 100 (expt 2 63)))      ;does not fit in signed 'long long'
           (item (string-append (%store-prefix)
                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-bad-size")))
      ;; Create fake substituter data, to be read by 'guix substitute'.
      (call-with-output-file (string-append (%substitute-directory)
                                            "/" (store-path-hash-part item)
                                            ".narinfo")
        (lambda (port)
          (format port "StorePath: ~a
URL: http://example.org
Compression: none
NarSize: ~a
NarHash: sha256:0fj9vhblff2997pi7qjj7lhmy7wzhnjwmkm2hmq6gr4fzmg10s0w
References: 
System: x86_64-linux~%"
                  item size)))

      ;; Remove entry from the local cache.
      (false-if-exception
       (delete-file-recursively (string-append (getenv "XDG_CACHE_HOME")
                                               "/guix/substitute")))

      ;; Make sure 'guix substitute' correctly communicates the above
      ;; data.
      (set-build-options s #:use-substitutes? #t
                         #:substitute-urls (%test-substitute-urls))
      (match (pk 'spi (substitutable-path-info s (list item)))
        (((? substitutable? s))
         (and (equal? (substitutable-path s) item)
              (substitutable-nar-size s)))))))

(test-equal "substitute and large size"
  (+ 100 (expt 2 31))                     ;<https://issues.guix.gnu.org/46212>
  (with-store s
    (let* ((size (+ 100 (expt 2 31)))            ;does not fit in signed 'int'
           (item (string-append (%store-prefix)
                                "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-bad-size-"
                                (random-text)))
           (nar  (string-append (%substitute-directory) "/nar")))
      ;; Create a dummy nar to allow for substitution.
      (call-with-output-file nar
        (lambda (port)
          (write-file-tree (store-path-package-name item) port
                           #:file-type+size (lambda _
                                              (values 'regular 12))
                           #:file-port (lambda _
                                         (open-input-string "Hello world.")))))

      ;; Create fake substituter data, to be read by 'guix substitute'.
      (call-with-output-file (string-append (%substitute-directory)
                                            "/" (store-path-hash-part item)
                                            ".narinfo")
        (lambda (port)
          (format port "StorePath: ~a
URL: file://~a
Compression: none
NarSize: ~a
NarHash: sha256:~a
References: 
System: x86_64-linux~%"
                  item nar size
                  (bytevector->nix-base32-string (gcrypt:file-sha256 nar)))))

      ;; Remove entry from the local cache.
      (false-if-exception
       (delete-file-recursively (string-append (getenv "XDG_CACHE_HOME")
                                               "/guix/substitute")))

      ;; Make sure 'guix substitute' correctly communicates the above
      ;; data.
      (set-build-options s #:use-substitutes? #t
                         #:substitute-urls (%test-substitute-urls))
      (ensure-path s item)
      (path-info-nar-size (query-path-info s item)))))

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

(test-assert "export/import incomplete"
  (let* ((file0 (add-text-to-store %store "baz" (random-text)))
         (file1 (add-text-to-store %store "foo" (random-text)
                                   (list file0)))
         (file2 (add-text-to-store %store "bar" (random-text)
                                   (list file1)))
         (dump  (call-with-bytevector-output-port
                 (cute export-paths %store (list file2) <>))))
    (delete-paths %store (list file0 file1 file2))
    (guard (c ((store-protocol-error? c)
               (and (not (zero? (store-protocol-error-status c)))
                    (string-contains (store-protocol-error-message c)
                                     "not valid"))))
      ;; Here we get an exception because DUMP does not include FILE0 and
      ;; FILE1, which are dependencies of FILE2.
      (import-paths %store (open-bytevector-input-port dump)))))

(test-assert "export/import recursive"
  (let* ((file0 (add-text-to-store %store "baz" (random-text)))
         (file1 (add-text-to-store %store "foo" (random-text)
                                   (list file0)))
         (file2 (add-text-to-store %store "bar" (random-text)
                                   (list file1)))
         (dump  (call-with-bytevector-output-port
                 (cute export-paths %store (list file2) <>
                       #:recursive? #t))))
    (delete-paths %store (list file0 file1 file2))
    (let ((imported (import-paths %store (open-bytevector-input-port dump))))
      (and (equal? imported (list file0 file1 file2))
           (every file-exists? (list file0 file1 file2))
           (equal? (list file0) (references %store file1))
           (equal? (list file1) (references %store file2))))))

(test-assert "write-file & export-path yield the same result"
  ;; Here we compare 'write-file' and the daemon's own implementation.
  ;; 'write-file' is the reference because we know it sorts file
  ;; deterministically.  Conversely, the daemon uses 'readdir' and the entries
  ;; currently happen to be sorted as a side-effect of some unrelated
  ;; operation (search for 'unhacked' in archive.cc.)  Make sure we detect any
  ;; changes there.
  (run-with-store %store
    (mlet* %store-monad ((drv1 (package->derivation %bootstrap-guile))
                         (out1 -> (derivation->output-path drv1))
                         (data -> (unfold (cut >= <> 26)
                                          (lambda (i)
                                            (random-bytevector 128))
                                          1+ 0))
                         (build
                          -> #~(begin
                                 (use-modules (rnrs io ports) (srfi srfi-1))
                                 (let ()
                                   (define letters
                                     (map (lambda (i)
                                            (string
                                             (integer->char
                                              (+ i (char->integer #\a)))))
                                          (iota 26)))
                                   (define (touch file data)
                                     (call-with-output-file file
                                       (lambda (port)
                                         (put-bytevector port data))))

                                   (mkdir #$output)
                                   (chdir #$output)

                                   ;; The files must be different so they have
                                   ;; different inode numbers, and the inode
                                   ;; order must differ from the lexicographic
                                   ;; order.
                                   (for-each touch
                                             (append (drop letters 10)
                                                     (take letters 10))
                                             (list #$@data))
                                   #t)))
                         (drv2 (gexp->derivation "bunch" build))
                         (out2 -> (derivation->output-path drv2))
                         (item-info -> (store-lift query-path-info)))
      (mbegin %store-monad
        (built-derivations (list drv1 drv2))
        (foldm %store-monad
               (lambda (item result)
                 (define ref-hash
                   (let-values (((port get) (gcrypt:open-sha256-port)))
                     (write-file item port)
                     (close-port port)
                     (get)))

                 ;; 'query-path-info' returns a hash produced by using the
                 ;; daemon's C++ 'dump' function, which is the implementation
                 ;; under test.
                 (>>= (item-info item)
                      (lambda (info)
                        (return
                         (and result
                              (bytevector=? (path-info-hash info) ref-hash))))))
               #t
               (list out1 out2))))
    #:guile-for-build (%guile-for-build)))


(test-assert "import not signed"
  (let* ((text (random-text))
         (file (add-file-tree-to-store %store
                                       `("tree" directory
                                         ("text" regular (data ,text))
                                         ("link" symlink "text"))))
         (dump (call-with-bytevector-output-port
                (lambda (port)
                  (write-int 1 port)              ;start

                  (write-file file port)          ;contents
                  (write-int #x4558494e port)     ;%export-magic
                  (write-string file port)        ;store item
                  (write-string-list '() port)    ;references
                  (write-string "" port)          ;deriver
                  (write-int 0 port)              ;not signed

                  (write-int 0 port)))))          ;done

    ;; Ensure 'import-paths' raises an exception.
    (guard (c ((store-protocol-error? c)
               (and (not (zero? (store-protocol-error-status c)))
                    (string-contains (store-protocol-error-message c)
                                     "lacks a signature"))))
      (let* ((source   (open-bytevector-input-port dump))
             (imported (import-paths %store source)))
        (pk 'unsigned-imported imported)
        #f))))

(test-assert "import signed by unauthorized key"
  (let* ((text (random-text))
         (file (add-file-tree-to-store %store
                                       `("tree" directory
                                         ("text" regular (data ,text))
                                         ("link" symlink "text"))))
         (key  (gcrypt:generate-key
                (gcrypt:string->canonical-sexp
                 "(genkey (ecdsa (curve Ed25519) (flags rfc6979)))")))
         (dump (call-with-bytevector-output-port
                (lambda (port)
                  (write-int 1 port)              ;start

                  (write-file file port)          ;contents
                  (write-int #x4558494e port)     ;%export-magic
                  (write-string file port)        ;store item
                  (write-string-list '() port)    ;references
                  (write-string "" port)          ;deriver
                  (write-int 1 port)              ;signed
                  (write-string (gcrypt:canonical-sexp->string
                                 (signature-sexp
                                  (gcrypt:bytevector->hash-data
                                   (gcrypt:sha256 #vu8(0 1 2))
                                   #:key-type 'ecc)
                                  (gcrypt:find-sexp-token key 'private-key)
                                  (gcrypt:find-sexp-token key 'public-key)))
                                port)

                  (write-int 0 port)))))          ;done

    ;; Ensure 'import-paths' raises an exception.
    (guard (c ((store-protocol-error? c)
               (and (not (zero? (store-protocol-error-status c)))
                    (string-contains (store-protocol-error-message c)
                                     "unauthorized public key"))))
      (let* ((source   (open-bytevector-input-port dump))
             (imported (import-paths %store source)))
        (pk 'unauthorized-imported imported)
        #f))))

(test-assert "import corrupt path"
  (let* ((text (random-text))
         (file (add-text-to-store %store "text" text))
         (dump (call-with-bytevector-output-port
                (cut export-paths %store (list file) <>))))
    (delete-paths %store (list file))

    ;; Flip a bit in the stream's payload.  INDEX here falls in the middle of
    ;; the file contents in DUMP, regardless of the store prefix.
    (let* ((index #x70)
           (byte  (bytevector-u8-ref dump index)))
      (bytevector-u8-set! dump index (logxor #xff byte)))

    (and (not (file-exists? file))
         (guard (c ((store-protocol-error? c)
                    (pk 'c c)
                    (and (not (zero? (store-protocol-error-status c)))
                         (string-contains (store-protocol-error-message c)
                                          "corrupt"))))
           (let* ((source   (open-bytevector-input-port dump))
                  (imported (import-paths %store source)))
             (pk 'corrupt-imported imported)
             #f)))))

(test-assert "verify-store"
  (let* ((text  (random-text))
         (file1 (add-text-to-store %store "foo" text))
         (file2 (add-text-to-store %store "bar" (random-text)
                                   (list file1))))
    (and (pk 'verify1 (verify-store %store))    ;hopefully OK ;
         (begin
           (delete-file file1)
           (not (pk 'verify2 (verify-store %store)))) ;bad! ;
         (begin
           ;; Using 'add-text-to-store' here wouldn't work: It would succeed ;
           ;; without actually creating the file. ;
           (call-with-output-file file1
             (lambda (port)
               (display text port)))
           (pk 'verify3 (verify-store %store)))))) ;OK again

(test-assert "verify-store + check-contents"
  ;; XXX: This test is I/O intensive.
  (with-store s
    (let* ((text (random-text))
           (drv  (build-expression->derivation
                  s "corrupt"
                  `(let ((out (assoc-ref %outputs "out")))
                     (call-with-output-file out
                       (lambda (port)
                         (display ,text port)))
                     #t)
                  #:guile-for-build
                  (package-derivation s %bootstrap-guile (%current-system))))
           (file (derivation->output-path drv)))
      (with-derivation-substitute drv text
        (and (build-derivations s (list drv))
             (verify-store s #:check-contents? #t) ;should be OK
             (begin
               (chmod file #o644)
               (call-with-output-file file
                 (lambda (port)
                   (display "corrupt!" port)))
               #t)

             ;; Make sure the corruption is detected.  We don't test repairing
             ;; because only "trusted" users are allowed to do it, but we
             ;; don't expose that notion of trusted users that nix-daemon
             ;; supports because it seems dubious and redundant with what the
             ;; OS provides (in Nix "trusted" users have additional
             ;; privileges, such as overriding the set of substitute URLs, but
             ;; we instead want to allow anyone to modify them, provided
             ;; substitutes are signed by a root-approved key.)
             (not (verify-store s #:check-contents? #t))

             ;; Delete the corrupt item to leave the store in a clean state.
             (delete-paths s (list file)))))))

(test-assert "build-things, check mode"
  (with-store store
    (call-with-temporary-output-file
     (lambda (entropy entropy-port)
       (write (random-text) entropy-port)
       (force-output entropy-port)
       (let* ((drv  (build-expression->derivation
                     store "non-deterministic"
                     `(begin
                        (use-modules (rnrs io ports))
                        (let ((out (assoc-ref %outputs "out")))
                          (call-with-output-file out
                            (lambda (port)
                              ;; Rely on the fact that tests do not use the
                              ;; chroot, and thus ENTROPY is readable.
                              (display (call-with-input-file ,entropy
                                         get-string-all)
                                       port)))
                          #t))
                     #:guile-for-build
                     (package-derivation store %bootstrap-guile (%current-system))))
              (file (derivation->output-path drv)))
         (and (build-things store (list (derivation-file-name drv)))
              (begin
                (write (random-text) entropy-port)
                (force-output entropy-port)
                (guard (c ((store-protocol-error? c)
                           (pk 'determinism-exception c)
                           (and (not (zero? (store-protocol-error-status c)))
                                (string-contains (store-protocol-error-message c)
                                                 "deterministic"))))
                  ;; This one will produce a different result.  Since we're in
                  ;; 'check' mode, this must fail.
                  (build-things store (list (derivation-file-name drv))
                                (build-mode check))
                  #f))))))))

(test-assert "build-succeeded trace in check mode"
  (string-contains
   (call-with-output-string
     (lambda (port)
       (let ((d (build-expression->derivation
                 %store "foo" '(mkdir (assoc-ref %outputs "out"))
                 #:guile-for-build
                 (package-derivation %store %bootstrap-guile))))
         (build-derivations %store (list d))
         (parameterize ((current-build-output-port port))
           (build-derivations %store (list d) (build-mode check))))))
   "@ build-succeeded"))

(test-assert "build multiple times"
  (with-store store
    ;; Ask to build twice.
    (set-build-options store #:rounds 2 #:use-substitutes? #f)

    (call-with-temporary-output-file
     (lambda (entropy entropy-port)
       (write (random-text) entropy-port)
       (force-output entropy-port)
       (let* ((drv  (build-expression->derivation
                     store "non-deterministic"
                     `(begin
                        (use-modules (rnrs io ports))
                        (let ((out (assoc-ref %outputs "out")))
                          (call-with-output-file out
                            (lambda (port)
                              ;; Rely on the fact that tests do not use the
                              ;; chroot, and thus ENTROPY is accessible.
                              (display (call-with-input-file ,entropy
                                         get-string-all)
                                       port)
                              (call-with-output-file ,entropy
                                (lambda (port)
                                  (write 'foobar port)))))
                          #t))
                     #:guile-for-build
                     (package-derivation store %bootstrap-guile (%current-system))))
              (file (derivation->output-path drv)))
         (guard (c ((store-protocol-error? c)
                    (pk 'multiple-build c)
                    (and (not (zero? (store-protocol-error-status c)))
                         (string-contains (store-protocol-error-message c)
                                          "deterministic"))))
           ;; This one will produce a different result on the second run.
           (current-build-output-port (current-error-port))
           (build-things store (list (derivation-file-name drv)))
           #f))))))

(test-equal "store-lower"
  "Lowered."
  (let* ((add  (store-lower text-file))
         (file (add %store "foo" "Lowered.")))
    (call-with-input-file file get-string-all)))

(test-equal "current-system"
  "bar"
  (parameterize ((%current-system "frob"))
    (run-with-store %store
      (mbegin %store-monad
        (set-current-system "bar")
        (current-system))
      #:system "foo")))

(test-assert "query-path-info"
  (let* ((ref (add-text-to-store %store "ref" "foo"))
         (item (add-text-to-store %store "item" "bar" (list ref)))
         (info (query-path-info %store item)))
    (and (equal? (path-info-references info) (list ref))
         (equal? (path-info-hash info)
                 (gcrypt:sha256
                  (string->utf8
                   (call-with-output-string (cut write-file item <>))))))))

(test-assert "path-info-deriver"
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
         (not (path-info-deriver (query-path-info %store b)))
         (string=? (derivation-file-name d)
                   (path-info-deriver (query-path-info %store o))))))

(test-equal "build-cores"
  (list 0 42)
  (with-store store
    (let* ((build  (add-text-to-store store "build.sh"
                                      "echo $NIX_BUILD_CORES > $out"))
           (bash   (add-to-store store "bash" #t "sha256"
                                 (search-bootstrap-binary "bash"
                                                          (%current-system))))
           (drv1   (derivation store "the-thing" bash
                               `("-e" ,build)
                               #:inputs `((,bash) (,build))
                               #:env-vars `(("x" . ,(random-text)))))
           (drv2   (derivation store "the-thing" bash
                               `("-e" ,build)
                               #:inputs `((,bash) (,build))
                               #:env-vars `(("x" . ,(random-text))))))
      (and (build-derivations store (list drv1))
           (begin
             (set-build-options store #:build-cores 42)
             (build-derivations store (list drv2)))
           (list (call-with-input-file (derivation->output-path drv1)
                   read)
                 (call-with-input-file (derivation->output-path drv2)
                   read))))))

(test-equal "multiplexed-build-output"
  '("Hello from first." "Hello from second.")
  (with-store store
    (let* ((build  (add-text-to-store store "build.sh"
                                      "echo Hello from $NAME.; echo > $out"))
           (bash   (add-to-store store "bash" #t "sha256"
                                 (search-bootstrap-binary "bash"
                                                          (%current-system))))
           (drv1   (derivation store "one" bash
                               `("-e" ,build)
                               #:inputs `((,bash) (,build))
                               #:env-vars `(("NAME" . "first")
                                            ("x" . ,(random-text)))))
           (drv2   (derivation store "two" bash
                               `("-e" ,build)
                               #:inputs `((,bash) (,build))
                               #:env-vars `(("NAME" . "second")
                                            ("x" . ,(random-text))))))
      (set-build-options store
                         #:print-build-trace #t
                         #:multiplexed-build-output? #t
                         #:max-build-jobs 10)
      (let ((port (open-output-string)))
        ;; Send the build log to PORT.
        (parameterize ((current-build-output-port port))
          (build-derivations store (list drv1 drv2)))

        ;; Retrieve the build log; make sure it contains valid "@ build-log"
        ;; traces that allow us to retrieve each builder's output (we assume
        ;; there's exactly one "build-output" trace for each builder, which is
        ;; reasonable.)
        (let* ((log     (get-output-string port))
               (started (fold-matches
                         (make-regexp "@ build-started ([^ ]+) - ([^ ]+) ([^ ]+) ([0-9]+)")
                         log '() cons))
               (done    (fold-matches
                         (make-regexp "@ build-succeeded (.*) - (.*) (.*) (.*)")
                         log '() cons))
               (output  (fold-matches
                         (make-regexp "@ build-log ([[:digit:]]+) ([[:digit:]]+)\n([A-Za-z .*]+)\n")
                         log '() cons))
               (drv-pid (lambda (name)
                          (lambda (m)
                            (let ((drv (match:substring m 1))
                                  (pid (string->number
                                        (match:substring m 4))))
                              (and (string-suffix? name drv) pid)))))
               (pid-log (lambda (pid)
                          (lambda (m)
                            (let ((n   (string->number
                                        (match:substring m 1)))
                                  (len (string->number
                                        (match:substring m 2)))
                                  (str (match:substring m 3)))
                              (and (= pid n)
                                   (= (string-length str) (- len 1))
                                   str)))))
               (pid1    (any (drv-pid "one.drv") started))
               (pid2    (any (drv-pid "two.drv") started)))
          (list (any (pid-log pid1) output)
                (any (pid-log pid2) output)))))))

(test-end "store")
