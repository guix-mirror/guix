;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-derivations)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix tests)
  #:use-module ((guix packages) #:select (package-derivation base32))
  #:use-module ((guix build utils) #:select (executable-file?))
  #:use-module ((gnu packages) #:select (search-bootstrap-binary))
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
         (d1 (read-derivation (open-bytevector-input-port b1)))
         (b2 (call-with-bytevector-output-port (cut write-derivation d1 <>)))
         (d2 (read-derivation (open-bytevector-input-port b2))))
    (and (equal? b1 b2)
         (equal? d1 d2))))

(test-skip (if %store 0 12))

(test-assert "add-to-store, flat"
  (let* ((file (search-path %load-path "language/tree-il/spec.scm"))
         (drv  (add-to-store %store "flat-test" #f "sha256" file)))
    (and (eq? 'regular (stat:type (stat drv)))
         (valid-path? %store drv)
         (equal? (call-with-input-file file get-bytevector-all)
                 (call-with-input-file drv get-bytevector-all)))))

(test-assert "add-to-store, recursive"
  (let* ((dir (dirname (search-path %load-path "language/tree-il/spec.scm")))
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
                              #:inputs `((,%bash) (,builder))))
         (succeeded?
          (build-derivations %store (list drv))))
    (and succeeded?
         (let ((path (derivation->output-path drv)))
           (and (valid-path? %store path)
                (string=? (call-with-input-file path read-line)
                          "hello, world"))))))

(test-assert "derivation with local file as input"
  (let* ((builder    (add-text-to-store
                      %store "my-builder.sh"
                      "(while read line ; do echo \"$line\" ; done) < $in > $out"
                      '()))
         (input      (search-path %load-path "ice-9/boot-9.scm"))
         (input*     (add-to-store %store (basename input)
                                   #t "sha256" input))
         (drv        (derivation %store "derivation-with-input-file"
                                 %bash `(,builder)

                                 ;; Cheat to pass the actual file name to the
                                 ;; builder.
                                 #:env-vars `(("in" . ,input*))

                                 #:inputs `((,%bash)
                                            (,builder)
                                            (,input))))) ; ← local file name
    (and (build-derivations %store (list drv))
         ;; Note: we can't compare the files because the above trick alters
         ;; the contents.
         (valid-path? %store (derivation->output-path drv)))))

(test-assert "identical files are deduplicated"
  (let* ((build1  (add-text-to-store %store "one.sh"
                                     "echo hello, world > \"$out\"\n"
                                     '()))
         (build2  (add-text-to-store %store "two.sh"
                                     "# Hey!\necho hello, world > \"$out\"\n"
                                     '()))
         (drv1    (derivation %store "foo"
                              %bash `(,build1)
                              #:inputs `((,%bash) (,build1))))
         (drv2    (derivation %store "bar"
                              %bash `(,build2)
                              #:inputs `((,%bash) (,build2)))))
    (and (build-derivations %store (list drv1 drv2))
         (let ((file1 (derivation->output-path drv1))
               (file2 (derivation->output-path drv2)))
           (and (valid-path? %store file1) (valid-path? %store file2)
                (string=? (call-with-input-file file1 get-string-all)
                          "hello, world\n")
                (= (stat:ino (lstat file1))
                   (stat:ino (lstat file2))))))))

(test-assert "offloadable-derivation?"
  (and (offloadable-derivation? (derivation %store "foo" %bash '()))
       (not (offloadable-derivation?
             (derivation %store "foo" %bash '()
                         #:local-build? #t)))))

(test-assert "fixed-output-derivation?"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo -n hello > $out" '()))
         (hash       (sha256 (string->utf8 "hello")))
         (drv        (derivation %store "fixed"
                                 %bash `(,builder)
                                 #:inputs `((,builder))
                                 #:hash hash #:hash-algo 'sha256)))
    (fixed-output-derivation? drv)))

(test-assert "fixed-output derivation"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo -n hello > $out" '()))
         (hash       (sha256 (string->utf8 "hello")))
         (drv        (derivation %store "fixed"
                                 %bash `(,builder)
                                 #:inputs `((,builder)) ; optional
                                 #:hash hash #:hash-algo 'sha256))
         (succeeded? (build-derivations %store (list drv))))
    (and succeeded?
         (let ((p (derivation->output-path drv)))
           (and (equal? (string->utf8 "hello")
                        (call-with-input-file p get-bytevector-all))
                (bytevector? (query-path-hash %store p)))))))

(test-assert "fixed-output derivation: output paths are equal"
  (let* ((builder1   (add-text-to-store %store "fixed-builder1.sh"
                                        "echo -n hello > $out" '()))
         (builder2   (add-text-to-store %store "fixed-builder2.sh"
                                        "echo hey; echo -n hello > $out" '()))
         (hash       (sha256 (string->utf8 "hello")))
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
         (hash       (sha256 (string->utf8 "hello")))
         (drv        (derivation %store "fixed-rec"
                                 %bash `(,builder)
                                 #:inputs `((,builder))
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
         (hash       (sha256 (string->utf8 "hello")))
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
                                 #:inputs `((,%bash) (,builder3) (,fixed1))))
         (final2     (derivation %store "final"
                                 %bash `(,builder3)
                                 #:env-vars `(("in" . ,fixed-out))
                                 #:inputs `((,%bash) (,builder3) (,fixed2))))
         (succeeded? (build-derivations %store
                                        (list final1 final2))))
    (and succeeded?
         (equal? (derivation->output-path final1)
                 (derivation->output-path final2)))))

(test-assert "multiple-output derivation"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo one > $out ; echo two > $second"
                                        '()))
         (drv        (derivation %store "fixed"
                                 %bash `(,builder)
                                 #:env-vars '(("HOME" . "/homeless")
                                              ("zzz"  . "Z!")
                                              ("AAA"  . "A!"))
                                 #:inputs `((,%bash) (,builder))
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
                                 #:inputs `((,%bash) (,builder))
                                 #:outputs '("out" "AAA")))
         (succeeded? (build-derivations %store (list drv))))
    (and succeeded?
         (let ((one (derivation->output-path drv "out"))
               (two (derivation->output-path drv "AAA")))
           (and (eq? 'one (call-with-input-file one read))
                (eq? 'two (call-with-input-file two read)))))))

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
                                 #:inputs `((,%bash) (,builder1))
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
                                 #:inputs `((,%bash)
                                            (,builder2)
                                            ;; two occurrences of MDRV:
                                            (,mdrv)
                                            (,mdrv "two")))))
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
                              #:inputs `((,%bash) (,builder))))
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
                         #:inputs `((,%bash))
                         #:allowed-references '())))
    (build-derivations %store (list drv))))

(test-assert "derivation #:allowed-references, not allowed"
  (let* ((txt (add-text-to-store %store "foo" "Hello, world."))
         (drv (derivation %store "disallowed" %bash
                          `("-c" ,(string-append "echo " txt "> $out"))
                          #:inputs `((,%bash) (,txt))
                          #:allowed-references '())))
    (guard (c ((nix-protocol-error? c)
               ;; There's no specific error message to check for.
               #t))
      (build-derivations %store (list drv))
      #f)))

(test-assert "derivation #:allowed-references, self allowed"
  (let ((drv (derivation %store "allowed" %bash
                         '("-c" "echo $out > $out")
                         #:inputs `((,%bash))
                         #:allowed-references '("out"))))
    (build-derivations %store (list drv))))

(test-assert "derivation #:allowed-references, self not allowed"
  (let ((drv (derivation %store "disallowed" %bash
                         `("-c" ,"echo $out > $out")
                         #:inputs `((,%bash))
                         #:allowed-references '())))
    (guard (c ((nix-protocol-error? c)
               ;; There's no specific error message to check for.
               #t))
      (build-derivations %store (list drv))
      #f)))


(define %coreutils
  (false-if-exception
   (and (getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)
        (or (package-derivation %store %bootstrap-coreutils&co)
            (nixpkgs-derivation "coreutils")))))

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
                      #:inputs `((,builder)
                                 (,%coreutils))))
         (succeeded?
          (build-derivations %store (list drv))))
    (and succeeded?
         (let ((p (derivation->output-path drv)))
           (and (valid-path? %store p)
                (file-exists? (string-append p "/good")))))))

(test-skip (if (%guile-for-build) 0 8))

(test-assert "build-expression->derivation and derivation-prerequisites"
  (let ((drv (build-expression->derivation %store "fail" #f)))
    (any (match-lambda
          (($ <derivation-input> path)
           (string=? path (derivation-file-name (%guile-for-build)))))
         (derivation-prerequisites drv))))

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
    (guard (c ((nix-protocol-error? c)
               (and (string-contains (nix-protocol-error-message c)
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
    (guard (c ((nix-protocol-error? c)
               (and (string-contains (nix-protocol-error-message c)
                                     "failed")
                    (not (valid-path? store out-path)))))
      (build-derivations store (list drv))
      #f)))

(test-assert "build-expression->derivation and derivation-prerequisites-to-build"
  (let ((drv (build-expression->derivation %store "fail" #f)))
    ;; The only direct dependency is (%guile-for-build) and it's already
    ;; built.
    (null? (derivation-prerequisites-to-build %store drv))))

(test-assert "derivation-prerequisites-to-build when outputs already present"
  (let* ((builder    '(begin (mkdir %output) #t))
         (input-drv  (build-expression->derivation %store "input" builder))
         (input-path (derivation-output-path
                      (assoc-ref (derivation-outputs input-drv)
                                 "out")))
         (drv        (build-expression->derivation %store "something" builder
                                                   #:inputs
                                                   `(("i" ,input-drv))))
         (output     (derivation->output-path drv)))
    ;; Make sure these things are not already built.
    (when (valid-path? %store input-path)
      (delete-paths %store (list input-path)))
    (when (valid-path? %store output)
      (delete-paths %store (list output)))

    (and (equal? (map derivation-input-path
                      (derivation-prerequisites-to-build %store drv))
                 (list (derivation-file-name input-drv)))

         ;; Build DRV and delete its input.
         (build-derivations %store (list drv))
         (delete-paths %store (list input-path))
         (not (valid-path? %store input-path))

         ;; Now INPUT-PATH is missing, yet it shouldn't be listed as a
         ;; prerequisite to build because DRV itself is already built.
         (null? (derivation-prerequisites-to-build %store drv)))))

(test-skip (if (getenv "GUIX_BINARY_SUBSTITUTE_URL") 0 1))
(test-assert "derivation-prerequisites-to-build and substitutes"
  (let* ((store  (open-connection))
         (drv    (build-expression->derivation store "prereq-subst"
                                               (random 1000)))
         (output (derivation->output-path drv)))

    ;; Make sure substitutes are usable.
    (set-build-options store #:use-substitutes? #t)

    (with-derivation-narinfo drv
      (let-values (((build download)
                    (derivation-prerequisites-to-build store drv))
                   ((build* download*)
                    (derivation-prerequisites-to-build store drv
                                                       #:use-substitutes? #f)))
        (and (null? build)
             (equal? download (list output))
             (null? download*)
             (null? build*))))))

(test-assert "build-expression->derivation with expression returning #f"
  (let* ((builder  '(begin
                      (mkdir %output)
                      #f))                        ; fail!
         (drv      (build-expression->derivation %store "fail" builder))
         (out-path (derivation->output-path drv)))
    (guard (c ((nix-protocol-error? c)
               ;; Note that the output path may exist at this point, but it
               ;; is invalid.
               (and (string-match "build .* failed"
                                  (nix-protocol-error-message c))
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

(test-assert "imported-files"
  (let* ((files    `(("x"     . ,(search-path %load-path "ice-9/q.scm"))
                     ("a/b/c" . ,(search-path %load-path
                                              "guix/derivations.scm"))
                     ("p/q"   . ,(search-path %load-path "guix.scm"))
                     ("p/z"   . ,(search-path %load-path "guix/store.scm"))))
         (drv      (imported-files %store files)))
    (and (build-derivations %store (list drv))
         (let ((dir (derivation->output-path drv)))
           (every (match-lambda
                   ((path . source)
                    (equal? (call-with-input-file (string-append dir "/" path)
                              get-bytevector-all)
                            (call-with-input-file source
                              get-bytevector-all))))
                  files)))))

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
         (hash       (sha256 (string->utf8 "hello")))
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
         (hash       (sha256 (string->utf8 "hello")))
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


(test-assert "graft-derivation"
  (let* ((build `(begin
                   (mkdir %output)
                   (chdir %output)
                   (symlink %output "self")
                   (call-with-output-file "text"
                     (lambda (output)
                       (format output "foo/~a/bar" ,%mkdir)))
                   (symlink ,%bash "sh")))
         (orig  (build-expression->derivation %store "graft" build
                                              #:inputs `(("a" ,%bash)
                                                         ("b" ,%mkdir))))
         (one   (add-text-to-store %store "bash" "fake bash"))
         (two   (build-expression->derivation %store "mkdir"
                                              '(call-with-output-file %output
                                                 (lambda (port)
                                                   (display "fake mkdir" port)))))
         (graft (graft-derivation %store "graft" orig
                                  (list (graft
                                          (origin %bash)
                                          (replacement one))
                                        (graft
                                          (origin %mkdir)
                                          (replacement two))))))
    (and (build-derivations %store (list graft))
         (let ((two   (derivation->output-path two))
               (graft (derivation->output-path graft)))
           (and (string=? (format #f "foo/~a/bar" two)
                          (call-with-input-file (string-append graft "/text")
                            get-string-all))
                (string=? (readlink (string-append graft "/sh")) one)
                (string=? (readlink (string-append graft "/self")) graft))))))

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
                                #:inputs `((,bash-full) (,script1))))
         (drv2      (map-derivation %store drv1
                                    `((,bash-full . ,%bash)
                                      (,script1 . ,script2))))
         (out       (derivation->output-path drv2)))
    (and (build-derivations %store (list (pk 'remapped* drv2)))
         (call-with-input-file out get-string-all))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;; Local Variables:
;; eval: (put 'with-derivation-narinfo 'scheme-indent-function 1)
;; End:
