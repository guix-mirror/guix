;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (test-utils)
  #:use-module ((guix config) #:select (%gzip))
  #:use-module (guix utils)
  #:use-module ((guix store) #:select (%store-prefix store-path-package-name))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist))

(define temp-file
  (string-append "t-utils-" (number->string (getpid))))

(test-begin "utils")

(test-assert "bytevector->base16-string->bytevector"
  (every (lambda (bv)
           (equal? (base16-string->bytevector
                    (bytevector->base16-string bv))
                   bv))
         (map string->utf8 '("" "f" "fo" "foo" "foob" "fooba" "foobar"))))

(test-assert "gnu-triplet->nix-system"
  (let ((samples '(("i586-gnu0.3" "i686-gnu")
                   ("x86_64-unknown-linux-gnu" "x86_64-linux")
                   ("i386-pc-linux-gnu" "i686-linux")
                   ("x86_64-unknown-freebsd8.2" "x86_64-freebsd")
                   ("x86_64-apple-darwin10.8.0" "x86_64-darwin")
                   ("i686-pc-cygwin" "i686-cygwin"))))
    (let-values (((gnu nix) (unzip2 samples)))
      (every (lambda (gnu nix)
               (equal? nix (gnu-triplet->nix-system gnu)))
             gnu nix))))

(test-assert "package-name->name+version"
  (every (match-lambda
          ((name version)
           (let*-values (((full-name)
                          (if version
                              (string-append name "-" version)
                              name))
                         ((name* version*)
                          (package-name->name+version full-name)))
             (and (equal? name* name)
                  (equal? version* version)))))
         '(("foo" "0.9.1b")
           ("foo-bar" "1.0")
           ("foo-bar2" #f)
           ("guile" "2.0.6.65-134c9") ; as produced by `git-version-gen'
           ("nixpkgs" "1.0pre22125_a28fe19")
           ("gtk2" "2.38.0"))))

(test-assert "guile-version>? 1.8"
  (guile-version>? "1.8"))

(test-assert "guile-version>? 10.5"
  (not (guile-version>? "10.5")))

(test-equal "string-tokenize*"
  '(("foo")
    ("foo" "bar" "baz")
    ("foo" "bar" "")
    ("foo" "bar" "baz"))
  (list (string-tokenize* "foo" ":")
        (string-tokenize* "foo;bar;baz" ";")
        (string-tokenize* "foo!bar!" "!")
        (string-tokenize* "foo+-+bar+-+baz" "+-+")))

(test-equal "string-replace-substring"
  '("foo BAR! baz"
    "/gnu/store/chbouib"
    "")
  (list (string-replace-substring "foo bar baz" "bar" "BAR!")
        (string-replace-substring "/nix/store/chbouib" "/nix/" "/gnu/")
        (string-replace-substring "" "foo" "bar")))

(test-equal "fold2, 1 list"
    (list (reverse (iota 5))
          (map - (reverse (iota 5))))
  (call-with-values
      (lambda ()
        (fold2 (lambda (i r1 r2)
                 (values (cons i r1)
                         (cons (- i) r2)))
               '() '()
               (iota 5)))
    list))

(test-equal "fold2, 2 lists"
    (list (reverse '((a . 0) (b . 1) (c . 2) (d . 3)))
          (reverse '((a . 0) (b . -1) (c . -2) (d . -3))))
  (call-with-values
      (lambda ()
        (fold2 (lambda (k v r1 r2)
                 (values (alist-cons k v r1)
                         (alist-cons k (- v) r2)))
               '() '()
               '(a b c d)
               '(0 1 2 3)))
    list))

(let* ((tree (alist->vhash
              '((0 2 3) (1 3 4) (2) (3 5 6) (4 6) (5) (6))
              hashq))
       (add-one (lambda (_ r) (1+ r)))
       (tree-lookup (lambda (n) (cdr (vhash-assq n tree)))))
  (test-equal "fold-tree, single root"
    5 (fold-tree add-one 0 tree-lookup '(0)))
  (test-equal "fold-tree, two roots"
    7 (fold-tree add-one 0 tree-lookup '(0 1)))
  (test-equal "fold-tree, sum"
    16 (fold-tree + 0 tree-lookup '(0)))
  (test-equal "fold-tree, internal"
    18 (fold-tree + 0 tree-lookup '(3 4)))
  (test-equal "fold-tree, cons"
    '(1 3 4 5 6)
    (sort (fold-tree cons '() tree-lookup '(1)) <))
  (test-equal "fold-tree, overlapping paths"
    '(1 3 4 5 6)
    (sort (fold-tree cons '() tree-lookup '(1 4)) <))
  (test-equal "fold-tree, cons, two roots"
    '(0 2 3 4 5 6)
    (sort (fold-tree cons '() tree-lookup '(0 4)) <))
  (test-equal "fold-tree-leaves, single root"
    2 (fold-tree-leaves add-one 0 tree-lookup '(1)))
  (test-equal "fold-tree-leaves, single root, sum"
    11 (fold-tree-leaves + 0 tree-lookup '(1)))
  (test-equal "fold-tree-leaves, two roots"
    3 (fold-tree-leaves add-one 0 tree-lookup '(0 1)))
  (test-equal "fold-tree-leaves, two roots, sum"
    13 (fold-tree-leaves + 0 tree-lookup '(0 1))))

(test-assert "filtered-port, file"
  (let* ((file  (search-path %load-path "guix.scm"))
         (input (open-file file "r0b")))
    (let*-values (((compressed pids1)
                   (filtered-port `(,%gzip "-c" "--fast") input))
                  ((decompressed pids2)
                   (filtered-port `(,%gzip "-d") compressed)))
      (and (every (compose zero? cdr waitpid)
                  (append pids1 pids2))
           (equal? (get-bytevector-all decompressed)
                   (call-with-input-file file get-bytevector-all))))))

(test-assert "filtered-port, non-file"
  (let ((data (call-with-input-file (search-path %load-path "guix.scm")
                get-bytevector-all)))
    (let*-values (((compressed pids1)
                   (filtered-port `(,%gzip "-c" "--fast")
                                  (open-bytevector-input-port data)))
                  ((decompressed pids2)
                   (filtered-port `(,%gzip "-d") compressed)))
      (and (pk (every (compose zero? cdr waitpid)
                   (append pids1 pids2)))
           (equal? (get-bytevector-all decompressed) data)))))

(test-assert "filtered-port, does not exist"
  (let* ((file  (search-path %load-path "guix.scm"))
         (input (open-file file "r0b")))
    (let-values (((port pids)
                  (filtered-port '("/does/not/exist") input)))
      (any (compose (negate zero?) cdr waitpid)
           pids))))

(test-assert "compressed-port, decompressed-port, non-file"
  (let ((data (call-with-input-file (search-path %load-path "guix.scm")
                get-bytevector-all)))
    (let*-values (((compressed pids1)
                   (compressed-port 'xz (open-bytevector-input-port data)))
                  ((decompressed pids2)
                   (decompressed-port 'xz compressed)))
      (and (every (compose zero? cdr waitpid)
                  (append pids1 pids2))
           (equal? (get-bytevector-all decompressed) data)))))

(false-if-exception (delete-file temp-file))
(test-assert "compressed-output-port + decompressed-port"
  (let* ((file (search-path %load-path "guix/derivations.scm"))
         (data (call-with-input-file file get-bytevector-all))
         (port (open-file temp-file "w0b")))
    (call-with-compressed-output-port 'xz port
      (lambda (compressed)
        (put-bytevector compressed data)))
    (close-port port)

    (bytevector=? data
                  (call-with-decompressed-port 'xz (open-file temp-file "r0b")
                    get-bytevector-all))))

(false-if-exception (delete-file temp-file))
(test-equal "fcntl-flock wait"
  42                                              ; the child's exit status
  (let ((file (open-file temp-file "w0b")))
    ;; Acquire an exclusive lock.
    (fcntl-flock file 'write-lock)
    (match (primitive-fork)
      (0
       (dynamic-wind
         (const #t)
         (lambda ()
           ;; Reopen FILE read-only so we can have a read lock.
           (let ((file (open-file temp-file "r0b")))
             ;; Wait until we can acquire the lock.
             (fcntl-flock file 'read-lock)
             (primitive-exit (read file)))
           (primitive-exit 1))
         (lambda ()
           (primitive-exit 2))))
      (pid
       ;; Write garbage and wait.
       (display "hello, world!"  file)
       (force-output file)
       (sleep 1)

       ;; Write the real answer.
       (seek file 0 SEEK_SET)
       (truncate-file file 0)
       (write 42 file)
       (force-output file)

       ;; Unlock, which should let the child continue.
       (fcntl-flock file 'unlock)

       (match (waitpid pid)
         ((_  . status)
          (let ((result (status:exit-val status)))
            (close-port file)
            result)))))))

(test-equal "fcntl-flock non-blocking"
  EAGAIN                                          ; the child's exit status
  (match (pipe)
    ((input . output)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #t)
          (lambda ()
            (close-port output)

            ;; Wait for the green light.
            (read-char input)

            ;; Open FILE read-only so we can have a read lock.
            (let ((file (open-file temp-file "w0")))
              (catch 'flock-error
                (lambda ()
                  ;; This attempt should throw EAGAIN.
                  (fcntl-flock file 'write-lock #:wait? #f))
                (lambda (key errno)
                  (primitive-exit (pk 'errno errno)))))
            (primitive-exit -1))
          (lambda ()
            (primitive-exit -2))))
       (pid
        (close-port input)
        (let ((file (open-file temp-file "w0")))
          ;; Acquire an exclusive lock.
          (fcntl-flock file 'write-lock)

          ;; Tell the child to continue.
          (write 'green-light output)
          (force-output output)

          (match (waitpid pid)
            ((_  . status)
             (let ((result (status:exit-val status)))
               (fcntl-flock file 'unlock)
               (close-port file)
               result)))))))))

;; This is actually in (guix store).
(test-equal "store-path-package-name"
  "bash-4.2-p24"
  (store-path-package-name
   (string-append (%store-prefix)
                  "/qvs2rj2ia5vci3wsdb7qvydrmacig4pg-bash-4.2-p24")))

(test-end)

(false-if-exception (delete-file temp-file))


(exit (= (test-runner-fail-count (test-runner-current)) 0))
