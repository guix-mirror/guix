;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(unsetenv "http_proxy")

(define-module (test-derivations)
  #:use-module (guix derivations)
  #:use-module (guix grafts)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module ((gcrypt hash) #:prefix gcrypt:)
  #:use-module (guix base32)
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module ((guix packages) #:select (package-derivation base32))
  #:use-module ((guix build utils) #:select (executable-file?))
  #:use-module (gnu packages bootstrap)
  #:use-module ((gnu packages guile) #:select (guile-1.8))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (web uri)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match))

(define %store
  (open-connection-for-tests))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)

(define (bootstrap-binary name)
  (let ((bin (search-bootstrap-binary name (%current-system))))
    (and %store
         (add-to-store %store name #t "sha256" bin))))

(define %bash
  (bootstrap-binary "bash"))
(define %mkdir
  (bootstrap-binary "mkdir"))

(define* (directory-contents dir #:optional (slurp get-bytevector-all))
  "Return an alist representing the contents of DIR."
  (define prefix-len (string-length dir))
  (sort (file-system-fold (const #t)                   ; enter?
                          (lambda (path stat result)   ; leaf
                            (alist-cons (string-drop path prefix-len)
                                        (call-with-input-file path slurp)
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
         (d1 (read-derivation (open-bytevector-input-port b1)
                              identity))
         (b2 (call-with-bytevector-output-port (cut write-derivation d1 <>)))
         (d2 (read-derivation (open-bytevector-input-port b2)
                              identity)))
    (and (equal? b1 b2)
         (equal? d1 d2))))

(test-skip (if %store 0 12))

(test-assert "add-to-store, flat"
  ;; Use 'readlink*' in case spec.scm is a symlink, as is the case when Guile
  ;; was installed with Stow.
  (let* ((file (readlink*
                (search-path %load-path "language/tree-il/spec.scm")))
         (drv  (add-to-store %store "flat-test" #f "sha256" file)))
    (and (eq? 'regular (stat:type (stat drv)))
         (valid-path? %store drv)
         (equal? (call-with-input-file file get-bytevector-all)
                 (call-with-input-file drv get-bytevector-all)))))

(test-assert "add-to-store, recursive"
  (let* ((dir (dirname
               (readlink* (search-path %load-path
                                       "language/tree-il/spec.scm"))))
         (drv (add-to-store %store "dir-tree-test" #t "sha256" dir)))
    (and (eq? 'directory (stat:type (stat drv)))
         (valid-path? %store drv)
         (equal? (directory-contents dir)
                 (directory-contents drv)))))

(test-assert "derivation with no inputs"
  (let* ((builder  (add-text-to-store %store "my-builder.sh"
                                      "echo hello, world\n"
                                      '()))
         (drv      (derivation %store "foo"
                               %bash `("-e" ,builder)
                               #:env-vars '(("HOME" . "/homeless")))))
    (and (store-path? (derivation-file-name drv))
         (valid-path? %store (derivation-file-name drv)))))

(test-assert "build derivation with 1 source"
  (let* ((builder (add-text-to-store %store "my-builder.sh"
                                     "echo hello, world > \"$out\"\n"
                                     '()))
         (drv     (derivation %store "foo"
                              %bash `(,builder)
                              #:env-vars '(("HOME" . "/homeless")
                                           ("zzz"  . "Z!")
                                           ("AAA"  . "A!"))
                              #:sources `(,%bash ,builder)))
         (succeeded?
          (build-derivations %store (list drv))))
    (and succeeded?
         (let ((path (derivation->output-path drv)))
           (and (valid-path? %store path)
                (string=? (call-with-input-file path read-line)
                          "hello, world"))))))

(test-assert "derivation fails but keep going"
  ;; In keep-going mode, 'build-derivations' should fail because of D1, but it
  ;; must return only after D2 has succeeded.
  (with-store store
    (let* ((d1 (derivation %store "fails"
                           %bash `("-c" "false")
                           #:sources (list %bash)))
           (d2 (build-expression->derivation %store "sleep-then-succeed"
                                             `(begin
                                                ,(random-text)
                                                ;; XXX: Hopefully that's long
                                                ;; enough that D1 has already
                                                ;; failed.
                                                (sleep 2)
                                                (mkdir %output)))))
      (set-build-options %store
                         #:use-substitutes? #f
                         #:keep-going? #t)
      (guard (c ((store-protocol-error? c)
                 (and (= 100 (store-protocol-error-status c))
                      (string-contains (store-protocol-error-message c)
                                       (derivation-file-name d1))
                      (not (valid-path? %store (derivation->output-path d1)))
                      (valid-path? %store (derivation->output-path d2)))))
        (build-derivations %store (list d1 d2))
        #f))))

(test-assert "identical files are deduplicated"
  (let* ((build1  (add-text-to-store %store "one.sh"
                                     "echo hello, world > \"$out\"\n"
                                     '()))
         (build2  (add-text-to-store %store "two.sh"
                                     "# Hey!\necho hello, world > \"$out\"\n"
                                     '()))
         (drv1    (derivation %store "foo"
                              %bash `(,build1)
                              #:sources `(,%bash ,build1)))
         (drv2    (derivation %store "bar"
                              %bash `(,build2)
                              #:sources `(,%bash ,build2))))
    (and (build-derivations %store (list drv1 drv2))
         (let ((file1 (derivation->output-path drv1))
               (file2 (derivation->output-path drv2)))
           (and (valid-path? %store file1) (valid-path? %store file2)
                (string=? (call-with-input-file file1 get-string-all)
                          "hello, world\n")
                (= (stat:ino (lstat file1))
                   (stat:ino (lstat file2))))))))

(test-equal "built-in-builders"
  '("download")
  (built-in-builders %store))

(test-assert "unknown built-in builder"
  (let ((drv (derivation %store "ohoh" "builtin:does-not-exist" '())))
    (guard (c ((store-protocol-error? c)
               (string-contains (store-protocol-error-message c) "failed")))
      (build-derivations %store (list drv))
      #f)))

(test-assert "'download' built-in builder"
  (let ((text (random-text)))
    (with-http-server `((200 ,text))
      (let* ((drv (derivation %store "world"
                              "builtin:download" '()
                              #:env-vars `(("url"
                                            . ,(object->string (%local-url))))
                              #:hash-algo 'sha256
                              #:hash (gcrypt:sha256 (string->utf8 text)))))
        (and (build-derivations %store (list drv))
             (string=? (call-with-input-file (derivation->output-path drv)
                         get-string-all)
                       text))))))

(test-assert "'download' built-in builder, invalid hash"
  (with-http-server `((200 "hello, world!"))
    (let* ((drv (derivation %store "world"
                            "builtin:download" '()
                            #:env-vars `(("url"
                                          . ,(object->string (%local-url))))
                            #:hash-algo 'sha256
                            #:hash (gcrypt:sha256 (random-bytevector 100))))) ;wrong
      (guard (c ((store-protocol-error? c)
                 (string-contains (store-protocol-error-message c) "failed")))
        (build-derivations %store (list drv))
        #f))))

(test-assert "'download' built-in builder, not found"
  (with-http-server '((404 "not found"))
    (let* ((drv (derivation %store "will-never-be-found"
                            "builtin:download" '()
                            #:env-vars `(("url"
                                          . ,(object->string (%local-url))))
                            #:hash-algo 'sha256
                            #:hash (gcrypt:sha256 (random-bytevector 100)))))
      (guard (c ((store-protocol-error? c)
                 (string-contains (store-protocol-error-message (pk c)) "failed")))
        (build-derivations %store (list drv))
        #f))))

(test-assert "'download' built-in builder, not fixed-output"
  (let* ((source (add-text-to-store %store "hello" "hi!"))
         (url    (string-append "file://" source))
         (drv    (derivation %store "world"
                             "builtin:download" '()
                             #:env-vars `(("url" . ,(object->string url))))))
    (guard (c ((store-protocol-error? c)
               (string-contains (store-protocol-error-message c) "failed")))
      (build-derivations %store (list drv))
      #f)))

(test-assert "'download' built-in builder, check mode"
  ;; Make sure rebuilding the 'builtin:download' derivation in check mode
  ;; works.  See <http://bugs.gnu.org/25089>.
  (let* ((text (random-text)))
    (with-http-server `((200 ,text))
      (let ((drv (derivation %store "world"
                             "builtin:download" '()
                             #:env-vars `(("url"
                                           . ,(object->string (%local-url))))
                             #:hash-algo 'sha256
                             #:hash (gcrypt:sha256 (string->utf8 text)))))
        (and drv (build-derivations %store (list drv))
             (with-http-server `((200 ,text))
               (build-derivations %store (list drv)
                                  (build-mode check)))
             (string=? (call-with-input-file (derivation->output-path drv)
                         get-string-all)
                       text))))))

(test-equal "derivation-name"
  "foo-0.0"
  (let ((drv (derivation %store "foo-0.0" %bash '())))
    (derivation-name drv)))

(test-equal "derivation-output-names"
  '(("out") ("bar" "chbouib"))
  (let ((drv1 (derivation %store "foo-0.0" %bash '()))
        (drv2 (derivation %store "foo-0.0" %bash '()
                          #:outputs '("bar" "chbouib"))))
    (list (derivation-output-names drv1)
          (derivation-output-names drv2))))

(test-assert "offloadable-derivation?"
  (and (offloadable-derivation? (derivation %store "foo" %bash '()))
       (offloadable-derivation?               ;see <http://bugs.gnu.org/18747>
        (derivation %store "foo" %bash '()
                    #:substitutable? #f))
       (not (offloadable-derivation?
             (derivation %store "foo" %bash '()
                         #:local-build? #t)))))

(test-assert "substitutable-derivation?"
  (and (substitutable-derivation? (derivation %store "foo" %bash '()))
       (substitutable-derivation?             ;see <http://bugs.gnu.org/18747>
        (derivation %store "foo" %bash '()
                    #:local-build? #t))
       (not (substitutable-derivation?
             (derivation %store "foo" %bash '()
                         #:substitutable? #f)))))

(test-assert "fixed-output-derivation?"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo -n hello > $out" '()))
         (hash       (gcrypt:sha256 (string->utf8 "hello")))
         (drv        (derivation %store "fixed"
                                 %bash `(,builder)
                                 #:sources (list builder)
                                 #:hash hash #:hash-algo 'sha256)))
    (fixed-output-derivation? drv)))

(test-equal "fixed-output derivation"
  '(sha1 sha256 sha512)
  (map (lambda (hash-algorithm)
         (let* ((builder (add-text-to-store %store "my-fixed-builder.sh"
                                            "echo -n hello > $out" '()))
                (sha256  (gcrypt:sha256 (string->utf8 "hello")))
                (hash    (gcrypt:bytevector-hash
                          (string->utf8 "hello")
                          (gcrypt:lookup-hash-algorithm hash-algorithm)))
                (drv     (derivation %store
                                     (string-append
                                      "fixed-" (symbol->string hash-algorithm))
                                     %bash `(,builder)
                                     #:sources `(,builder) ;optional
                                     #:hash hash
                                     #:hash-algo hash-algorithm)))
           (build-derivations %store (list drv))
           (let ((p (derivation->output-path drv)))
             (and (bytevector=? (string->utf8 "hello")
                                (call-with-input-file p get-bytevector-all))
                  (bytevector? (query-path-hash %store p))
                  hash-algorithm))))
       '(sha1 sha256 sha512)))

(test-assert "fixed-output derivation: output paths are equal"
  (let* ((builder1   (add-text-to-store %store "fixed-builder1.sh"
                                        "echo -n hello > $out" '()))
         (builder2   (add-text-to-store %store "fixed-builder2.sh"
                                        "echo hey; echo -n hello > $out" '()))
         (hash       (gcrypt:sha256 (string->utf8 "hello")))
         (drv1       (derivation %store "fixed"
                                 %bash `(,builder1)
                                 #:hash hash #:hash-algo 'sha256))
         (drv2       (derivation %store "fixed"
                                 %bash `(,builder2)
                                 #:hash hash #:hash-algo 'sha256))
         (succeeded? (build-derivations %store (list drv1 drv2))))
    (and succeeded?
         (equal? (derivation->output-path drv1)
                 (derivation->output-path drv2)))))

(test-assert "fixed-output derivation, recursive"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo -n hello > $out" '()))
         (hash       (gcrypt:sha256 (string->utf8 "hello")))
         (drv        (derivation %store "fixed-rec"
                                 %bash `(,builder)
                                 #:sources (list builder)
                                 #:hash (base32 "0sg9f58l1jj88w6pdrfdpj5x9b1zrwszk84j81zvby36q9whhhqa")
                                 #:hash-algo 'sha256
                                 #:recursive? #t))
         (succeeded? (build-derivations %store (list drv))))
    (and succeeded?
         (let ((p (derivation->output-path drv)))
           (and (equal? (string->utf8 "hello")
                        (call-with-input-file p get-bytevector-all))
                (bytevector? (query-path-hash %store p)))))))

(test-assert "derivation with a fixed-output input"
  ;; A derivation D using a fixed-output derivation F doesn't has the same
  ;; output path when passed F or F', as long as F and F' have the same output
  ;; path.
  (let* ((builder1   (add-text-to-store %store "fixed-builder1.sh"
                                        "echo -n hello > $out" '()))
         (builder2   (add-text-to-store %store "fixed-builder2.sh"
                                        "echo hey; echo -n hello > $out" '()))
         (hash       (gcrypt:sha256 (string->utf8 "hello")))
         (fixed1     (derivation %store "fixed"
                                 %bash `(,builder1)
                                 #:hash hash #:hash-algo 'sha256))
         (fixed2     (derivation %store "fixed"
                                 %bash `(,builder2)
                                 #:hash hash #:hash-algo 'sha256))
         (fixed-out  (derivation->output-path fixed1))
         (builder3   (add-text-to-store
                      %store "final-builder.sh"
                      ;; Use Bash hackery to avoid Coreutils.
                      "echo $in ; (read -u 3 c; echo $c) 3< $in > $out" '()))
         (final1     (derivation %store "final"
                                 %bash `(,builder3)
                                 #:env-vars `(("in" . ,fixed-out))
                                 #:sources (list %bash builder3)
                                 #:inputs (list (derivation-input fixed1))))
         (final2     (derivation %store "final"
                                 %bash `(,builder3)
                                 #:env-vars `(("in" . ,fixed-out))
                                 #:sources (list %bash builder3)
                                 #:inputs (list (derivation-input fixed2))))
         (succeeded? (build-derivations %store
                                        (list final1 final2))))
    (and succeeded?
         (equal? (derivation->output-path final1)
                 (derivation->output-path final2)))))

(test-assert "derivation with duplicate fixed-output inputs"
  ;; Here we create a derivation that has two inputs, both of which are
  ;; fixed-output leading to the same result.  This test ensures the hash of
  ;; that derivation is correctly computed, namely that duplicate inputs are
  ;; coalesced.  See <https://bugs.gnu.org/36777>.
  (let* ((builder1   (add-text-to-store %store "fixed-builder1.sh"
                                        "echo -n hello > $out" '()))
         (builder2   (add-text-to-store %store "fixed-builder2.sh"
                                        "echo hey; echo -n hello > $out" '()))
         (hash       (gcrypt:sha256 (string->utf8 "hello")))
         (fixed1     (derivation %store "fixed"
                                 %bash `(,builder1)
                                 #:hash hash #:hash-algo 'sha256))
         (fixed2     (derivation %store "fixed"
                                 %bash `(,builder2)
                                 #:hash hash #:hash-algo 'sha256))
         (builder3   (add-text-to-store %store "builder.sh"
                                        "echo fake builder"))
         (final      (derivation %store "final"
                                 %bash `(,builder3)
                                 #:sources (list %bash builder3)
                                 #:inputs (list (derivation-input fixed1)
                                                (derivation-input fixed2)))))
    (and (derivation? final)
         (match (derivation-inputs final)
           (((= derivation-input-derivation one)
             (= derivation-input-derivation two))
            (and (not (string=? (derivation-file-name one)
                                (derivation-file-name two)))
                 (string=? (derivation->output-path one)
                           (derivation->output-path two))))))))

(test-assert "multiple-output derivation"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo one > $out ; echo two > $second"
                                        '()))
         (drv        (derivation %store "fixed"
                                 %bash `(,builder)
                                 #:env-vars '(("HOME" . "/homeless")
                                              ("zzz"  . "Z!")
                                              ("AAA"  . "A!"))
                                 #:sources `(,%bash ,builder)
                                 #:outputs '("out" "second")))
         (succeeded? (build-derivations %store (list drv))))
    (and succeeded?
         (let ((one (derivation->output-path drv "out"))
               (two (derivation->output-path drv "second")))
           (and (lset= equal?
                       (derivation->output-paths drv)
                       `(("out" . ,one) ("second" . ,two)))
                (eq? 'one (call-with-input-file one read))
                (eq? 'two (call-with-input-file two read)))))))

(test-assert "multiple-output derivation, non-alphabetic order"
  ;; Here, the outputs are not listed in alphabetic order.  Yet, the store
  ;; path computation must reorder them first.
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo one > $out ; echo two > $AAA"
                                        '()))
         (drv        (derivation %store "fixed"
                                 %bash `(,builder)
                                 #:sources `(,%bash ,builder)
                                 #:outputs '("out" "AAA")))
         (succeeded? (build-derivations %store (list drv))))
    (and succeeded?
         (let ((one (derivation->output-path drv "out"))
               (two (derivation->output-path drv "AAA")))
           (and (eq? 'one (call-with-input-file one read))
                (eq? 'two (call-with-input-file two read)))))))

(test-assert "read-derivation vs. derivation"
  ;; Make sure 'derivation' and 'read-derivation' return objects that are
  ;; identical.
  (let* ((sources (unfold (cut >= <> 10)
                          (lambda (n)
                            (add-text-to-store %store
                                               (format #f "input~a" n)
                                               (random-text)))
                          1+
                          0))
         (inputs  (map (lambda (file)
                         (derivation %store "derivation-input"
                                     %bash '()
                                     #:sources `(,%bash ,file)))
                       sources))
         (builder (add-text-to-store %store "builder.sh"
                                     "echo one > $one ; echo two > $two"
                                     '()))
         (drv     (derivation %store "derivation"
                              %bash `(,builder)
                              #:sources `(,%bash ,builder ,@sources)
                              #:inputs (map derivation-input inputs)
                              #:outputs '("two" "one")))
         (drv*    (call-with-input-file (derivation-file-name drv)
                    read-derivation)))
    (equal? drv* drv)))

(test-assert "multiple-output derivation, derivation-path->output-path"
  (let* ((builder    (add-text-to-store %store "builder.sh"
                                        "echo one > $out ; echo two > $second"
                                        '()))
         (drv        (derivation %store "multiple"
                                 %bash `(,builder)
                                 #:outputs '("out" "second")))
         (drv-file   (derivation-file-name drv))
         (one        (derivation->output-path drv "out"))
         (two        (derivation->output-path drv "second"))
         (first      (derivation-path->output-path drv-file "out"))
         (second     (derivation-path->output-path drv-file "second")))
    (and (not (string=? one two))
         (string-suffix? "-second" two)
         (string=? first one)
         (string=? second two))))

(test-assert "user of multiple-output derivation"
  ;; Check whether specifying several inputs coming from the same
  ;; multiple-output derivation works.
  (let* ((builder1   (add-text-to-store %store "my-mo-builder.sh"
                                        "echo one > $out ; echo two > $two"
                                        '()))
         (mdrv       (derivation %store "multiple-output"
                                 %bash `(,builder1)
                                 #:sources (list %bash builder1)
                                 #:outputs '("out" "two")))
         (builder2   (add-text-to-store %store "my-mo-user-builder.sh"
                                        "read x < $one;
                                         read y < $two;
                                         echo \"($x $y)\" > $out"
                                        '()))
         (udrv       (derivation %store "multiple-output-user"
                                 %bash `(,builder2)
                                 #:env-vars `(("one"
                                               . ,(derivation->output-path
                                                   mdrv "out"))
                                              ("two"
                                               . ,(derivation->output-path
                                                   mdrv "two")))
                                 #:sources (list %bash builder2)
                                 ;; two occurrences of MDRV:
                                 #:inputs
                                 (list (derivation-input mdrv)
                                       (derivation-input mdrv '("two"))))))
    (and (build-derivations %store (list (pk 'udrv udrv)))
         (let ((p (derivation->output-path udrv)))
           (and (valid-path? %store p)
                (equal? '(one two) (call-with-input-file p read)))))))

(test-assert "derivation with #:references-graphs"
  (let* ((input1  (add-text-to-store %store "foo" "hello"
                                     (list %bash)))
         (input2  (add-text-to-store %store "bar"
                                     (number->string (random 7777))
                                     (list input1)))
         (builder (add-text-to-store %store "build-graph"
                                     (format #f "
~a $out
 (while read l ; do echo $l ; done) < bash > $out/bash
 (while read l ; do echo $l ; done) < input1 > $out/input1
 (while read l ; do echo $l ; done) < input2 > $out/input2"
                                             %mkdir)
                                     (list %mkdir)))
         (drv     (derivation %store "closure-graphs"
                              %bash `(,builder)
                              #:references-graphs
                              `(("bash" . ,%bash)
                                ("input1" . ,input1)
                                ("input2" . ,input2))
                              #:sources (list %bash builder)))
         (out     (derivation->output-path drv)))
    (define (deps path . deps)
      (let ((count (length deps)))
        (string-append path "\n\n" (number->string count) "\n"
                       (string-join (sort deps string<?) "\n")
                       (if (zero? count) "" "\n"))))

    (and (build-derivations %store (list drv))
         (equal? (directory-contents out get-string-all)
                 `(("/bash"   . ,(string-append %bash "\n\n0\n"))
                   ("/input1" . ,(if (string>? input1 %bash)
                                     (string-append (deps %bash)
                                                    (deps input1 %bash))
                                     (string-append (deps input1 %bash)
                                                    (deps %bash))))
                   ("/input2" . ,(string-concatenate
                                  (map cdr
                                       (sort
                                        (map (lambda (p d)
                                               (cons p (apply deps p d)))
                                             (list %bash input1 input2)
                                             (list '() (list %bash) (list input1)))
                                        (lambda (x y)
                                          (match x
                                            ((p1 . _)
                                             (match y
                                               ((p2 . _)
                                                (string<? p1 p2)))))))))))))))

(test-assert "derivation #:allowed-references, ok"
  (let ((drv (derivation %store "allowed" %bash
                         '("-c" "echo hello > $out")
                         #:sources (list %bash)
                         #:allowed-references '())))
    (build-derivations %store (list drv))))

(test-assert "derivation #:allowed-references, not allowed"
  (let* ((txt (add-text-to-store %store "foo" "Hello, world."))
         (drv (derivation %store "disallowed" %bash
                          `("-c" ,(string-append "echo " txt "> $out"))
                          #:sources (list %bash txt)
                          #:allowed-references '())))
    (guard (c ((store-protocol-error? c)
               ;; There's no specific error message to check for.
               #t))
      (build-derivations %store (list drv))
      #f)))

(test-assert "derivation #:allowed-references, self allowed"
  (let ((drv (derivation %store "allowed" %bash
                         '("-c" "echo $out > $out")
                         #:sources (list %bash)
                         #:allowed-references '("out"))))
    (build-derivations %store (list drv))))

(test-assert "derivation #:allowed-references, self not allowed"
  (let ((drv (derivation %store "disallowed" %bash
                         `("-c" ,"echo $out > $out")
                         #:sources (list %bash)
                         #:allowed-references '())))
    (guard (c ((store-protocol-error? c)
               ;; There's no specific error message to check for.
               #t))
      (build-derivations %store (list drv))
      #f)))

(test-assert "derivation #:disallowed-references, ok"
  (let ((drv (derivation %store "disallowed" %bash
                         '("-c" "echo hello > $out")
                         #:sources (list %bash)
                         #:disallowed-references '("out"))))
    (build-derivations %store (list drv))))

(test-assert "derivation #:disallowed-references, not ok"
  (let* ((txt (add-text-to-store %store "foo" "Hello, world."))
         (drv (derivation %store "disdisallowed" %bash
                          `("-c" ,(string-append "echo " txt "> $out"))
                          #:sources (list %bash txt)
                          #:disallowed-references (list txt))))
    (guard (c ((store-protocol-error? c)
               ;; There's no specific error message to check for.
               #t))
      (build-derivations %store (list drv))
      #f)))

;; Here we should get the value of $GUIX_STATE_DIRECTORY that the daemon sees,
;; which is a unique value for each test process; this value is the same as
;; the one we see in the process executing this file since it is set by
;; 'test-env'.
(test-equal "derivation #:leaked-env-vars"
  (getenv "GUIX_STATE_DIRECTORY")
  (let* ((value (getenv "GUIX_STATE_DIRECTORY"))
         (drv   (derivation %store "leaked-env-vars" %bash
                            '("-c" "echo -n $GUIX_STATE_DIRECTORY > $out")
                            #:hash (gcrypt:sha256 (string->utf8 value))
                            #:hash-algo 'sha256
                            #:sources (list %bash)
                            #:leaked-env-vars '("GUIX_STATE_DIRECTORY"))))
    (and (build-derivations %store (list drv))
         (call-with-input-file (derivation->output-path drv)
           get-string-all))))


(define %coreutils
  (false-if-exception
   (and (network-reachable?)
        (package-derivation %store %bootstrap-coreutils&co))))

(test-skip (if %coreutils 0 1))

(test-assert "build derivation with coreutils"
  (let* ((builder
          (add-text-to-store %store "build-with-coreutils.sh"
                             "echo $PATH ; mkdir --version ; mkdir $out ; touch $out/good"
                             '()))
         (drv
          (derivation %store "foo"
                      %bash `(,builder)
                      #:env-vars `(("PATH" .
                                    ,(string-append
                                      (derivation->output-path %coreutils)
                                      "/bin")))
                      #:sources (list builder)
                      #:inputs (list (derivation-input %coreutils))))
         (succeeded?
          (build-derivations %store (list drv))))
    (and succeeded?
         (let ((p (derivation->output-path drv)))
           (and (valid-path? %store p)
                (file-exists? (string-append p "/good")))))))

(test-skip (if (%guile-for-build) 0 8))

(test-equal "build-expression->derivation and invalid module name"
  '(file-search-error "guix/module/that/does/not/exist.scm")
  (guard (c ((file-search-error? c)
             (list 'file-search-error
                   (file-search-error-file-name c))))
    (build-expression->derivation %store "foo" #t
                                  #:modules '((guix module that
                                                    does not exist)))))

(test-equal "build-expression->derivation and builder encoding"
  '("UTF-8" #t)
  (let* ((exp '(λ (α) (+ α 1)))
         (drv (build-expression->derivation %store "foo" exp)))
    (match (derivation-builder-arguments drv)
      ((... builder)
       (with-fluids ((%default-port-encoding "UTF-8"))
         (call-with-input-file builder
           (lambda (port)
             (list (port-encoding port)
                   (->bool
                    (string-contains (get-string-all port)
                                     "(λ (α) (+ α 1))"))))))))))

(test-assert "build-expression->derivation and derivation-prerequisites"
  (let ((drv (build-expression->derivation %store "fail" #f)))
    (any (match-lambda
          (($ <derivation-input> (= derivation-file-name path))
           (string=? path (derivation-file-name (%guile-for-build)))))
         (derivation-prerequisites drv))))

(test-assert "derivation-prerequisites and valid-derivation-input?"
  (let* ((a (build-expression->derivation %store "a" '(mkdir %output)))
         (b (build-expression->derivation %store "b" `(list ,(random-text))))
         (c (build-expression->derivation %store "c" `(mkdir %output)
                                          #:inputs `(("a" ,a) ("b" ,b)))))
    ;; Make sure both A and %BOOTSTRAP-GUILE are built (the latter could have
    ;; be removed by tests/guix-gc.sh.)
    (build-derivations %store
                       (list a (package-derivation %store %bootstrap-guile)))

    (match (derivation-prerequisites c
                                     (cut valid-derivation-input? %store
                                          <>))
      ((($ <derivation-input> (= derivation-file-name file) ("out")))
       (string=? file (derivation-file-name b)))
      (x
       (pk 'fail x #f)))))

(test-assert "build-expression->derivation without inputs"
  (let* ((builder    '(begin
                        (mkdir %output)
                        (call-with-output-file (string-append %output "/test")
                          (lambda (p)
                            (display '(hello guix) p)))))
         (drv        (build-expression->derivation %store "goo" builder))
         (succeeded? (build-derivations %store (list drv))))
    (and succeeded?
         (let ((p (derivation->output-path drv)))
           (equal? '(hello guix)
                   (call-with-input-file (string-append p "/test") read))))))

(test-assert "build-expression->derivation and max-silent-time"
  (let* ((store      (let ((s (open-connection)))
                       (set-build-options s #:max-silent-time 1)
                       s))
         (builder    '(begin (sleep 100) (mkdir %output) #t))
         (drv        (build-expression->derivation store "silent" builder))
         (out-path   (derivation->output-path drv)))
    (guard (c ((store-protocol-error? c)
               (and (string-contains (store-protocol-error-message c)
                                     "failed")
                    (not (valid-path? store out-path)))))
      (build-derivations store (list drv))
      #f)))

(test-assert "build-expression->derivation and timeout"
  (let* ((store      (let ((s (open-connection)))
                       (set-build-options s #:timeout 1)
                       s))
         (builder    '(begin (sleep 100) (mkdir %output) #t))
         (drv        (build-expression->derivation store "slow" builder))
         (out-path   (derivation->output-path drv)))
    (guard (c ((store-protocol-error? c)
               (and (string-contains (store-protocol-error-message c)
                                     "failed")
                    (not (valid-path? store out-path)))))
      (build-derivations store (list drv))
      #f)))

(test-assert "build-derivations with specific output"
  (with-store store
    (let* ((content (random-text))                ;contents of the output
           (drv     (build-expression->derivation
                     store "substitute-me"
                     `(begin ,content (exit 1))   ;would fail
                     #:outputs '("out" "one" "two")
                     #:guile-for-build
                     (package-derivation store %bootstrap-guile)))
           (out     (derivation->output-path drv)))
      (with-derivation-substitute drv content
        (set-build-options store #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? store out)

             ;; Ask for nothing but the "out" output of DRV.
             (build-derivations store `((,drv . "out")))

             ;; Synonymous:
             (build-derivations store (list (derivation-input drv '("out"))))

             (valid-path? store out)
             (equal? (pk 'x content)
                     (pk 'y (call-with-input-file out get-string-all))))))))

(test-assert "build-expression->derivation and derivation-build-plan"
  (let ((drv (build-expression->derivation %store "fail" #f)))
    ;; The only direct dependency is (%guile-for-build) and it's already
    ;; built.
    (null? (derivation-build-plan %store (derivation-inputs drv)))))

(test-assert "derivation-build-plan when outputs already present"
  (let* ((builder    `(begin ,(random-text) (mkdir %output) #t))
         (input-drv  (build-expression->derivation %store "input" builder))
         (input-path (derivation->output-path input-drv))
         (drv        (build-expression->derivation %store "something" builder
                                                   #:inputs
                                                   `(("i" ,input-drv))))
         (output     (derivation->output-path drv)))
    ;; Assume these things are not already built.
    (when (or (valid-path? %store input-path)
              (valid-path? %store output))
      (error "things already built" input-drv))

    (and (lset= equal?
                (map derivation-file-name
                     (derivation-build-plan %store
                                            (list (derivation-input drv))))
                (list (derivation-file-name input-drv)
                      (derivation-file-name drv)))

         ;; Build DRV and delete its input.
         (build-derivations %store (list drv))
         (delete-paths %store (list input-path))
         (not (valid-path? %store input-path))

         ;; Now INPUT-PATH is missing, yet it shouldn't be listed as a
         ;; prerequisite to build because DRV itself is already built.
         (null? (derivation-build-plan %store
                                       (list (derivation-input drv)))))))

(test-assert "derivation-build-plan and substitutes"
  (let* ((store  (open-connection))
         (drv    (build-expression->derivation store "prereq-subst"
                                               (random 1000)))
         (output (derivation->output-path drv)))

    ;; Make sure substitutes are usable.
    (set-build-options store #:use-substitutes? #t
                       #:substitute-urls (%test-substitute-urls))

    (with-derivation-narinfo drv
      (let-values (((build download)
                    (derivation-build-plan store
                                           (list (derivation-input drv))))
                   ((build* download*)
                    (derivation-build-plan store
                                           (list (derivation-input drv))
                                           #:substitutable-info
                                           (const #f))))
        (and (null? build)
             (equal? (map substitutable-path download) (list output))
             (null? download*)
             (equal? (list drv) build*))))))

(test-assert "derivation-build-plan and substitutes, non-substitutable build"
  (let* ((store  (open-connection))
         (drv    (build-expression->derivation store "prereq-no-subst"
                                               (random 1000)
                                               #:substitutable? #f))
         (output (derivation->output-path drv)))

    ;; Make sure substitutes are usable.
    (set-build-options store #:use-substitutes? #t
                       #:substitute-urls (%test-substitute-urls))

    (with-derivation-narinfo drv
      (let-values (((build download)
                    (derivation-build-plan store
                                           (list (derivation-input drv)))))
        ;; Despite being available as a substitute, DRV will be built locally
        ;; due to #:substitutable? #f.
        (and (null? download)
             (match build
               (((= derivation-file-name build))
                (string=? build (derivation-file-name drv)))))))))

(test-assert "derivation-build-plan and substitutes, non-substitutable dep"
  (with-store store
    (let* ((drv1 (build-expression->derivation store "prereq-no-subst"
                                               (random 1000)
                                               #:substitutable? #f))
           (drv2 (build-expression->derivation store "substitutable"
                                               (random 1000)
                                               #:inputs `(("dep" ,drv1)))))

      ;; Make sure substitutes are usable.
      (set-build-options store #:use-substitutes? #t
                         #:substitute-urls (%test-substitute-urls))

      (with-derivation-narinfo drv2
        (sha256 => (make-bytevector 32 0))
        (references => (list (derivation->output-path drv1)))

        (let-values (((build download)
                      (derivation-build-plan store
                                             (list (derivation-input drv2)))))
          ;; Although DRV2 is available as a substitute, we must build its
          ;; dependency, DRV1, due to #:substitutable? #f.
          (and (match download
                 (((= substitutable-path item))
                  (string=? item (derivation->output-path drv2))))
               (match build
                 (((= derivation-file-name build))
                  (string=? build (derivation-file-name drv1))))))))))

(test-assert "derivation-build-plan and substitutes, local build"
  (with-store store
    (let* ((drv    (build-expression->derivation store "prereq-subst-local"
                                                 (random 1000)
                                                 #:local-build? #t))
           (output (derivation->output-path drv)))

      ;; Make sure substitutes are usable.
      (set-build-options store #:use-substitutes? #t
                         #:substitute-urls (%test-substitute-urls))

      (with-derivation-narinfo drv
        (let-values (((build download)
                      (derivation-build-plan store
                                             (list (derivation-input drv)))))
          ;; #:local-build? is *not* synonymous with #:substitutable?, so we
          ;; must be able to substitute DRV's output.
          ;; See <http://bugs.gnu.org/18747>.
          (and (null? build)
               (match download
                 (((= substitutable-path item))
                  (string=? item (derivation->output-path drv))))))))))

(test-assert "derivation-build-plan in 'check' mode"
  (with-store store
    (let* ((dep (build-expression->derivation store "dep"
                                              `(begin ,(random-text)
                                                      (mkdir %output))))
           (drv (build-expression->derivation store "to-check"
                                              '(mkdir %output)
                                              #:inputs `(("dep" ,dep)))))
      (build-derivations store (list drv))
      (delete-paths store (list (derivation->output-path dep)))

      ;; In 'check' mode, DEP must be rebuilt.
      (and (null? (derivation-build-plan store
                                         (list (derivation-input drv))))
           (lset= equal?
                  (derivation-build-plan store
                                         (list (derivation-input drv))
                                         #:mode (build-mode check))
                  (list drv dep))))))

(test-assert "derivation-input-fold"
  (let* ((builder (add-text-to-store %store "my-builder.sh"
                                     "echo hello, world > \"$out\"\n"
                                     '()))
         (drv1    (derivation %store "foo"
                              %bash `(,builder)
                              #:sources `(,%bash ,builder)))
         (drv2    (derivation %store "bar"
                              %bash `(,builder)
                              #:inputs `((,drv1))
                              #:sources `(,%bash ,builder))))
    (equal? (derivation-input-fold (lambda (input result)
                                     (cons (derivation-input-derivation input)
                                           result))
                                   '()
                                   (list (derivation-input drv2)))
            (list drv1 drv2))))

(test-assert "substitution-oracle and #:substitute? #f"
  (with-store store
    (let* ((dep   (build-expression->derivation store "dep"
                                                `(begin ,(random-text)
                                                        (mkdir %output))))
           (drv   (build-expression->derivation store "not-subst"
                                                `(begin ,(random-text)
                                                        (mkdir %output))
                                                #:substitutable? #f
                                                #:inputs `(("dep" ,dep))))
           (query #f))
      (define (record-substitutable-path-query store paths)
        (when query
          (error "already called!" query))
        (set! query paths)
        '())

      (mock ((guix store) substitutable-path-info
             record-substitutable-path-query)

            (let ((pred (substitution-oracle store (list drv))))
              (pred (derivation->output-path drv))))

      ;; Make sure the oracle didn't try to get substitute info for DRV since
      ;; DRV is mark as non-substitutable.  Assume that GUILE-FOR-BUILD is
      ;; already in store and thus not part of QUERY.
      (equal? (pk 'query query)
              (list (derivation->output-path dep))))))

(test-assert "build-expression->derivation with expression returning #f"
  (let* ((builder  '(begin
                      (mkdir %output)
                      #f))                        ; fail!
         (drv      (build-expression->derivation %store "fail" builder))
         (out-path (derivation->output-path drv)))
    (guard (c ((store-protocol-error? c)
               ;; Note that the output path may exist at this point, but it
               ;; is invalid.
               (and (string-match "build .* failed"
                                  (store-protocol-error-message c))
                    (not (valid-path? %store out-path)))))
      (build-derivations %store (list drv))
      #f)))

(test-assert "build-expression->derivation with two outputs"
  (let* ((builder    '(begin
                        (call-with-output-file (assoc-ref %outputs "out")
                          (lambda (p)
                            (display '(hello) p)))
                        (call-with-output-file (assoc-ref %outputs "second")
                          (lambda (p)
                            (display '(world) p)))))
         (drv        (build-expression->derivation %store "double" builder
                                                   #:outputs '("out"
                                                               "second")))
         (succeeded? (build-derivations %store (list drv))))
    (and succeeded?
         (let ((one (derivation->output-path drv))
               (two (derivation->output-path drv "second")))
           (and (equal? '(hello) (call-with-input-file one read))
                (equal? '(world) (call-with-input-file two read)))))))

(test-skip (if %coreutils 0 1))
(test-assert "build-expression->derivation with one input"
  (let* ((builder    '(call-with-output-file %output
                        (lambda (p)
                          (let ((cu (assoc-ref %build-inputs "cu")))
                            (close 1)
                            (dup2 (port->fdes p) 1)
                            (execl (string-append cu "/bin/uname")
                                   "uname" "-a")))))
         (drv        (build-expression->derivation %store "uname" builder
                                                   #:inputs
                                                   `(("cu" ,%coreutils))))
         (succeeded? (build-derivations %store (list drv))))
    (and succeeded?
         (let ((p (derivation->output-path drv)))
           (string-contains (call-with-input-file p read-line) "GNU")))))

(test-assert "build-expression->derivation with modules"
  (let* ((builder  `(begin
                      (use-modules (guix build utils))
                      (let ((out (assoc-ref %outputs "out")))
                        (mkdir-p (string-append out "/guile/guix/nix"))
                        #t)))
         (drv      (build-expression->derivation %store "test-with-modules"
                                                 builder
                                                 #:modules
                                                 '((guix build utils)))))
    (and (build-derivations %store (list drv))
         (let* ((p (derivation->output-path drv))
                (s (stat (string-append p "/guile/guix/nix"))))
           (eq? (stat:type s) 'directory)))))

(test-assert "build-expression->derivation: same fixed-output path"
  (let* ((builder1   '(call-with-output-file %output
                        (lambda (p)
                          (write "hello" p))))
         (builder2   '(call-with-output-file (pk 'difference-here! %output)
                        (lambda (p)
                          (write "hello" p))))
         (hash       (gcrypt:sha256 (string->utf8 "hello")))
         (input1     (build-expression->derivation %store "fixed" builder1
                                                   #:hash hash
                                                   #:hash-algo 'sha256))
         (input2     (build-expression->derivation %store "fixed" builder2
                                                   #:hash hash
                                                   #:hash-algo 'sha256))
         (succeeded? (build-derivations %store (list input1 input2))))
    (and succeeded?
         (not (string=? (derivation-file-name input1)
                        (derivation-file-name input2)))
         (string=? (derivation->output-path input1)
                   (derivation->output-path input2)))))

(test-assert "build-expression->derivation with a fixed-output input"
  (let* ((builder1   '(call-with-output-file %output
                        (lambda (p)
                          (write "hello" p))))
         (builder2   '(call-with-output-file (pk 'difference-here! %output)
                        (lambda (p)
                          (write "hello" p))))
         (hash       (gcrypt:sha256 (string->utf8 "hello")))
         (input1     (build-expression->derivation %store "fixed" builder1
                                                   #:hash hash
                                                   #:hash-algo 'sha256))
         (input2     (build-expression->derivation %store "fixed" builder2
                                                   #:hash hash
                                                   #:hash-algo 'sha256))
         (builder3  '(let ((input (assoc-ref %build-inputs "input")))
                       (call-with-output-file %output
                         (lambda (out)
                           (format #f "My input is ~a.~%" input)))))
         (final1    (build-expression->derivation %store "final" builder3
                                                  #:inputs
                                                  `(("input" ,input1))))
         (final2    (build-expression->derivation %store "final" builder3
                                                  #:inputs
                                                  `(("input" ,input2)))))
    (and (string=? (derivation->output-path final1)
                   (derivation->output-path final2))
         (string=? (derivation->output-path final1)
                   (derivation-path->output-path
                    (derivation-file-name final1)))
         (build-derivations %store (list final1 final2)))))

(test-assert "build-expression->derivation produces recursive fixed-output"
  (let* ((builder '(begin
                     (use-modules (srfi srfi-26))
                     (mkdir %output)
                     (chdir %output)
                     (call-with-output-file "exe"
                       (cut display "executable" <>))
                     (chmod "exe" #o777)
                     (symlink "exe" "symlink")
                     (mkdir "subdir")))
         (drv     (build-expression->derivation %store "fixed-rec" builder
                                                #:hash-algo 'sha256
                                                #:hash (base32
                                                        "10k1lw41wyrjf9mxydi0is5nkpynlsvgslinics4ppir13g7d74p")
                                                #:recursive? #t)))
    (and (build-derivations %store (list drv))
         (let* ((dir    (derivation->output-path drv))
                (exe    (string-append dir "/exe"))
                (link   (string-append dir "/symlink"))
                (subdir (string-append dir "/subdir")))
           (and (executable-file? exe)
                (string=? "executable"
                          (call-with-input-file exe get-string-all))
                (string=? "exe" (readlink link))
                (file-is-directory? subdir))))))

(test-assert "build-expression->derivation uses recursive fixed-output"
  (let* ((builder '(call-with-output-file %output
                     (lambda (port)
                       (display "hello" port))))
         (fixed   (build-expression->derivation %store "small-fixed-rec"
                                                builder
                                                #:hash-algo 'sha256
                                                #:hash (base32
                                                        "0sg9f58l1jj88w6pdrfdpj5x9b1zrwszk84j81zvby36q9whhhqa")
                                                #:recursive? #t))
         (in      (derivation->output-path fixed))
         (builder `(begin
                     (mkdir %output)
                     (chdir %output)
                     (symlink ,in "symlink")))
         (drv     (build-expression->derivation %store "fixed-rec-user"
                                                builder
                                                #:inputs `(("fixed" ,fixed)))))
    (and (build-derivations %store (list drv))
         (let ((out (derivation->output-path drv)))
           (string=? (readlink (string-append out "/symlink")) in)))))

(test-assert "build-expression->derivation with #:references-graphs"
  (let* ((input   (add-text-to-store %store "foo" "hello"
                                     (list %bash %mkdir)))
         (builder '(copy-file "input" %output))
         (drv     (build-expression->derivation %store "references-graphs"
                                                builder
                                                #:references-graphs
                                                `(("input" . ,input))))
         (out     (derivation->output-path drv)))
    (define (deps path . deps)
      (let ((count (length deps)))
        (string-append path "\n\n" (number->string count) "\n"
                       (string-join (sort deps string<?) "\n")
                       (if (zero? count) "" "\n"))))

    (and (build-derivations %store (list drv))
         (equal? (call-with-input-file out get-string-all)
                 (string-concatenate
                  (map cdr
                       (sort (map (lambda (p d)
                                    (cons p (apply deps p d)))
                                  (list input %bash %mkdir)
                                  (list (list %bash %mkdir)
                                        '() '()))
                             (lambda (x y)
                               (match x
                                 ((p1 . _)
                                  (match y
                                    ((p2 . _)
                                     (string<? p1 p2)))))))))))))

(test-equal "derivation-properties"
  (list '() '((type . test)))
  (let ((drv1 (build-expression->derivation %store "bar"
                                            '(mkdir %output)))
        (drv2 (build-expression->derivation %store "foo"
                                           '(mkdir %output)
                                           #:properties '((type . test)))))
    (list (derivation-properties drv1)
          (derivation-properties drv2))))

(test-equal "map-derivation"
  "hello"
  (let* ((joke (package-derivation %store guile-1.8))
         (good (package-derivation %store %bootstrap-guile))
         (drv1 (build-expression->derivation %store "original-drv1"
                                             #f   ; systematically fail
                                             #:guile-for-build joke))
         (drv2 (build-expression->derivation %store "original-drv2"
                                             '(call-with-output-file %output
                                                (lambda (p)
                                                  (display "hello" p)))))
         (drv3 (build-expression->derivation %store "drv-to-remap"
                                             '(let ((in (assoc-ref
                                                         %build-inputs "in")))
                                                (copy-file in %output))
                                             #:inputs `(("in" ,drv1))
                                             #:guile-for-build joke))
         (drv4 (map-derivation %store drv3 `((,drv1 . ,drv2)
                                             (,joke . ,good))))
         (out  (derivation->output-path drv4)))
    (and (build-derivations %store (list (pk 'remapped drv4)))
         (call-with-input-file out get-string-all))))

(test-equal "map-derivation, sources"
  "hello"
  (let* ((script1   (add-text-to-store %store "fail.sh" "exit 1"))
         (script2   (add-text-to-store %store "hi.sh" "echo -n hello > $out"))
         (bash-full (package-derivation %store (@ (gnu packages bash) bash)))
         (drv1      (derivation %store "drv-to-remap"

                                ;; XXX: This wouldn't work in practice, but if
                                ;; we append "/bin/bash" then we can't replace
                                ;; it with the bootstrap bash, which is a
                                ;; single file.
                                (derivation->output-path bash-full)

                                `("-e" ,script1)
                                #:sources (list script1)
                                #:inputs
                                (list (derivation-input bash-full '("out")))))
         (drv2      (map-derivation %store drv1
                                    `((,bash-full . ,%bash)
                                      (,script1 . ,script2))))
         (out       (derivation->output-path drv2)))
    (and (build-derivations %store (list (pk 'remapped* drv2)))
         (call-with-input-file out get-string-all))))

(test-end)

;; Local Variables:
;; eval: (put 'with-http-server 'scheme-indent-function 1)
;; End:
