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


(define-module (test-derivations)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:use-module ((guix packages) #:select (package-derivation))
  #:use-module (distro packages bootstrap)
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

(when %store
  ;; Make sure we build everything by ourselves.
  (set-build-options %store #:use-substitutes? #f)

  ;; By default, use %BOOTSTRAP-GUILE for the current system.
  (let ((drv (package-derivation %store %bootstrap-guile)))
    (%guile-for-build drv)))

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

(test-skip (if %store 0 11))

(test-assert "add-to-store, flat"
  (let* ((file (search-path %load-path "language/tree-il/spec.scm"))
         (drv  (add-to-store %store "flat-test" #t #f "sha256" file)))
    (and (eq? 'regular (stat:type (stat drv)))
         (valid-path? %store drv)
         (equal? (call-with-input-file file get-bytevector-all)
                 (call-with-input-file drv get-bytevector-all)))))

(test-assert "add-to-store, recursive"
  (let* ((dir (dirname (search-path %load-path "language/tree-il/spec.scm")))
         (drv (add-to-store %store "dir-tree-test" #t #t "sha256" dir)))
    (and (eq? 'directory (stat:type (stat drv)))
         (valid-path? %store drv)
         (equal? (directory-contents dir)
                 (directory-contents drv)))))

(test-assert "derivation with no inputs"
  (let* ((builder  (add-text-to-store %store "my-builder.sh"
                                      "#!/bin/sh\necho hello, world\n"
                                      '()))
         (drv-path (derivation %store "foo" (%current-system) builder
                               '() '(("HOME" . "/homeless")) '())))
    (and (store-path? drv-path)
         (valid-path? %store drv-path))))

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
           (and (valid-path? %store path)
                (string=? (call-with-input-file path read-line)
                          "hello, world"))))))

(test-assert "derivation with local file as input"
  (let* ((builder    (add-text-to-store
                      %store "my-builder.sh"
                      "(while read line ; do echo $line ; done) < $in > $out"
                      '()))
         (input      (search-path %load-path "ice-9/boot-9.scm"))
         (drv-path   (derivation %store "derivation-with-input-file"
                                 (%current-system)
                                 "/bin/sh" `(,builder)
                                 `(("in"
                                    ;; Cheat to pass the actual file
                                    ;; name to the builder.
                                    . ,(add-to-store %store
                                                     (basename input)
                                                     #t #t "sha256"
                                                     input)))
                                 `((,builder)
                                   (,input)))))   ; ← local file name
    (and (build-derivations %store (list drv-path))
         (let ((p (derivation-path->output-path drv-path)))
           (and (call-with-input-file p get-bytevector-all)
                (call-with-input-file input get-bytevector-all))))))

(test-assert "fixed-output derivation"
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo -n hello > $out" '()))
         (hash       (sha256 (string->utf8 "hello")))
         (drv-path   (derivation %store "fixed" (%current-system)
                                 "/bin/sh" `(,builder)
                                 '()
                                 `((,builder))    ; optional
                                 #:hash hash #:hash-algo 'sha256))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((p (derivation-path->output-path drv-path)))
           (and (equal? (string->utf8 "hello")
                        (call-with-input-file p get-bytevector-all))
                (bytevector? (query-path-hash %store p)))))))

(test-assert "fixed-output derivation: output paths are equal"
  (let* ((builder1   (add-text-to-store %store "fixed-builder1.sh"
                                        "echo -n hello > $out" '()))
         (builder2   (add-text-to-store %store "fixed-builder2.sh"
                                        "echo hey; echo -n hello > $out" '()))
         (hash       (sha256 (string->utf8 "hello")))
         (drv-path1  (derivation %store "fixed" (%current-system)
                                 "/bin/sh" `(,builder1)
                                 '() `()
                                 #:hash hash #:hash-algo 'sha256))
         (drv-path2  (derivation %store "fixed" (%current-system)
                                 "/bin/sh" `(,builder2)
                                 '() `()
                                 #:hash hash #:hash-algo 'sha256))
         (succeeded? (build-derivations %store
                                        (list drv-path1 drv-path2))))
    (and succeeded?
         (equal? (derivation-path->output-path drv-path1)
                 (derivation-path->output-path drv-path2)))))

(test-assert "derivation with a fixed-output input"
  ;; A derivation D using a fixed-output derivation F doesn't has the same
  ;; output path when passed F or F', as long as F and F' have the same output
  ;; path.
  (let* ((builder1   (add-text-to-store %store "fixed-builder1.sh"
                                        "echo -n hello > $out" '()))
         (builder2   (add-text-to-store %store "fixed-builder2.sh"
                                        "echo hey; echo -n hello > $out" '()))
         (hash       (sha256 (string->utf8 "hello")))
         (fixed1     (derivation %store "fixed" (%current-system)
                                 "/bin/sh" `(,builder1)
                                 '() `()
                                 #:hash hash #:hash-algo 'sha256))
         (fixed2     (derivation %store "fixed" (%current-system)
                                 "/bin/sh" `(,builder2)
                                 '() `()
                                 #:hash hash #:hash-algo 'sha256))
         (fixed-out  (derivation-path->output-path fixed1))
         (builder3   (add-text-to-store
                      %store "final-builder.sh"
                      ;; Use Bash hackery to avoid Coreutils.
                      "echo $in ; (read -u 3 c; echo $c) 3< $in > $out" '()))
         (final1     (derivation %store "final" (%current-system)
                                 "/bin/sh" `(,builder3)
                                 `(("in" . ,fixed-out))
                                 `((,builder3) (,fixed1))))
         (final2     (derivation %store "final" (%current-system)
                                 "/bin/sh" `(,builder3)
                                 `(("in" . ,fixed-out))
                                 `((,builder3) (,fixed2))))
         (succeeded? (build-derivations %store
                                        (list final1 final2))))
    (and succeeded?
         (equal? (derivation-path->output-path final1)
                 (derivation-path->output-path final2)))))

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
           (and (lset= equal?
                       (derivation-path->output-paths drv-path)
                       `(("out" . ,one) ("second" . ,two)))
                (eq? 'one (call-with-input-file one read))
                (eq? 'two (call-with-input-file two read)))))))

(test-assert "multiple-output derivation, non-alphabetic order"
  ;; Here, the outputs are not listed in alphabetic order.  Yet, the store
  ;; path computation must reorder them first.
  (let* ((builder    (add-text-to-store %store "my-fixed-builder.sh"
                                        "echo one > $out ; echo two > $AAA"
                                        '()))
         (drv-path   (derivation %store "fixed" (%current-system)
                                 "/bin/sh" `(,builder)
                                 '()
                                 `((,builder))
                                 #:outputs '("out" "AAA")))
         (succeeded? (build-derivations %store (list drv-path))))
    (and succeeded?
         (let ((one (derivation-path->output-path drv-path "out"))
               (two (derivation-path->output-path drv-path "AAA")))
           (and (eq? 'one (call-with-input-file one read))
                (eq? 'two (call-with-input-file two read)))))))

(test-assert "user of multiple-output derivation"
  ;; Check whether specifying several inputs coming from the same
  ;; multiple-output derivation works.
  (let* ((builder1   (add-text-to-store %store "my-mo-builder.sh"
                                        "echo one > $out ; echo two > $two"
                                        '()))
         (mdrv       (derivation %store "multiple-output" (%current-system)
                                 "/bin/sh" `(,builder1)
                                 '()
                                 `((,builder1))
                                 #:outputs '("out" "two")))
         (builder2   (add-text-to-store %store "my-mo-user-builder.sh"
                                        "read x < $one;
                                         read y < $two;
                                         echo \"($x $y)\" > $out"
                                        '()))
         (udrv       (derivation %store "multiple-output-user"
                                 (%current-system)
                                 "/bin/sh" `(,builder2)
                                 `(("one" . ,(derivation-path->output-path
                                              mdrv "out"))
                                   ("two" . ,(derivation-path->output-path
                                              mdrv "two")))
                                 `((,builder2)
                                   ;; two occurrences of MDRV:
                                   (,mdrv)
                                   (,mdrv "two")))))
    (and (build-derivations %store (list (pk 'udrv udrv)))
         (let ((p (derivation-path->output-path udrv)))
           (and (valid-path? %store p)
                (equal? '(one two) (call-with-input-file p read)))))))


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
           (and (valid-path? %store p)
                (file-exists? (string-append p "/good")))))))

(test-skip (if (%guile-for-build) 0 7))

(test-assert "build-expression->derivation and derivation-prerequisites"
  (let-values (((drv-path drv)
                (build-expression->derivation %store "fail" (%current-system)
                                              #f '())))
    (any (match-lambda
          (($ <derivation-input> path)
           (string=? path (%guile-for-build))))
         (derivation-prerequisites drv))))

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

(test-assert "build-expression->derivation and derivation-prerequisites-to-build"
  (let-values (((drv-path drv)
                (build-expression->derivation %store "fail" (%current-system)
                                              #f '())))
    ;; The only direct dependency is (%guile-for-build) and it's already
    ;; built.
    (null? (derivation-prerequisites-to-build %store drv))))

(test-assert "derivation-prerequisites-to-build when outputs already present"
  (let*-values (((builder)
                 '(begin (mkdir %output) #t))
                ((input-drv-path input-drv)
                 (build-expression->derivation %store "input"
                                               (%current-system)
                                               builder '()))
                ((input-path)
                 (derivation-output-path
                  (assoc-ref (derivation-outputs input-drv)
                             "out")))
                ((drv-path drv)
                 (build-expression->derivation %store "something"
                                               (%current-system)
                                               builder
                                               `(("i" ,input-drv-path))))
                ((output)
                 (derivation-output-path
                  (assoc-ref (derivation-outputs drv) "out"))))
    ;; Make sure these things are not already built.
    (when (valid-path? %store input-path)
      (delete-paths %store (list input-path)))
    (when (valid-path? %store output)
      (delete-paths %store (list output)))

    (and (equal? (map derivation-input-path
                      (derivation-prerequisites-to-build %store drv))
                 (list input-drv-path))

         ;; Build DRV and delete its input.
         (build-derivations %store (list drv-path))
         (delete-paths %store (list input-path))
         (not (valid-path? %store input-path))

         ;; Now INPUT-PATH is missing, yet it shouldn't be listed as a
         ;; prerequisite to build because DRV itself is already built.
         (null? (derivation-prerequisites-to-build %store drv)))))

(test-assert "build-expression->derivation with expression returning #f"
  (let* ((builder  '(begin
                      (mkdir %output)
                      #f))                        ; fail!
         (drv-path (build-expression->derivation %store "fail" (%current-system)
                                                 builder '()))
         (out-path (derivation-path->output-path drv-path)))
    (guard (c ((nix-protocol-error? c)
               ;; Note that the output path may exist at this point, but it
               ;; is invalid.
               (and (string-match "build .* failed"
                                  (nix-protocol-error-message c))
                    (not (valid-path? %store out-path)))))
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

(test-skip (if %coreutils 0 1))
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

(test-assert "build-expression->derivation with modules"
  (let* ((builder  `(begin
                      (use-modules (guix build utils))
                      (let ((out (assoc-ref %outputs "out")))
                        (mkdir-p (string-append out "/guile/guix/nix"))
                        #t)))
         (drv-path (build-expression->derivation %store
                                                 "test-with-modules"
                                                 (%current-system)
                                                 builder '()
                                                 #:modules
                                                 '((guix build utils)))))
    (and (build-derivations %store (list drv-path))
         (let* ((p (derivation-path->output-path drv-path))
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
         (input1     (build-expression->derivation %store "fixed"
                                                   (%current-system)
                                                   builder1 '()
                                                   #:hash hash
                                                   #:hash-algo 'sha256))
         (input2     (build-expression->derivation %store "fixed"
                                                   (%current-system)
                                                   builder2 '()
                                                   #:hash hash
                                                   #:hash-algo 'sha256))
         (succeeded? (build-derivations %store (list input1 input2))))
    (and succeeded?
         (not (string=? input1 input2))
         (string=? (derivation-path->output-path input1)
                   (derivation-path->output-path input2)))))

(test-assert "build-expression->derivation with a fixed-output input"
  (let* ((builder1   '(call-with-output-file %output
                        (lambda (p)
                          (write "hello" p))))
         (builder2   '(call-with-output-file (pk 'difference-here! %output)
                        (lambda (p)
                          (write "hello" p))))
         (hash       (sha256 (string->utf8 "hello")))
         (input1     (build-expression->derivation %store "fixed"
                                                   (%current-system)
                                                   builder1 '()
                                                   #:hash hash
                                                   #:hash-algo 'sha256))
         (input2     (build-expression->derivation %store "fixed"
                                                   (%current-system)
                                                   builder2 '()
                                                   #:hash hash
                                                   #:hash-algo 'sha256))
         (builder3  '(let ((input (assoc-ref %build-inputs "input")))
                       (call-with-output-file %output
                         (lambda (out)
                           (format #f "My input is ~a.~%" input)))))
         (final1    (build-expression->derivation %store "final"
                                                  (%current-system)
                                                  builder3
                                                  `(("input" ,input1))))
         (final2    (build-expression->derivation %store "final"
                                                  (%current-system)
                                                  builder3
                                                  `(("input" ,input2)))))
    (and (string=? (derivation-path->output-path final1)
                   (derivation-path->output-path final2))
         (build-derivations %store (list final1 final2)))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; eval: (put 'guard 'scheme-indent-function 1)
;;; End:
