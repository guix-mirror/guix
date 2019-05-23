;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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
  #:use-module ((guix search-paths) #:select (string-tokenize*))
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
                              (string-append name "@" version)
                              name))
                         ((name* version*)
                          (package-name->name+version full-name)))
             (and (equal? name* name)
                  (equal? version* version)))))
         '(("foo" "0.9.1b")
           ("foo-14-bar" "320")
           ("foo-bar2" #f)
           ("guile" "2.0.6.65-134c9") ; as produced by `git-version-gen'
           ("nixpkgs" "1.0pre22125_a28fe19")
           ("gtk2" "2.38.0"))))

(test-assert "guile-version>? 1.8"
  (guile-version>? "1.8"))

(test-assert "guile-version>? 10.5"
  (not (guile-version>? "10.5")))

(test-assert "version-prefix?"
  (and (version-prefix? "4.1" "4.1.2")
       (version-prefix? "4.1" "4.1")
       (not (version-prefix? "4.1" "4.16.2"))
       (not (version-prefix? "4.1" "4"))))

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

(test-equal "strip-keyword-arguments"
  '(a #:b b #:c c)
  (strip-keyword-arguments '(#:foo #:bar #:baz)
                           '(a #:foo 42 #:b b #:baz 3
                               #:c c #:bar 4)))

(test-equal "ensure-keyword-arguments"
  '((#:foo 2)
    (#:foo 2 #:bar 3)
    (#:foo 42 #:bar 3))
  (list (ensure-keyword-arguments '(#:foo 2) '(#:foo 2))
        (ensure-keyword-arguments '(#:foo 2) '(#:bar 3))
        (ensure-keyword-arguments '(#:foo 2) '(#:bar 3 #:foo 42))))

(test-equal "default-keyword-arguments"
  '((#:foo 2)
    (#:foo 2)
    (#:foo 2 #:bar 3)
    (#:foo 2 #:bar 3)
    (#:foo 2 #:bar 3))
  (list (default-keyword-arguments '() '(#:foo 2))
        (default-keyword-arguments '(#:foo 2) '(#:foo 4))
        (default-keyword-arguments '() '(#:bar 3 #:foo 2))
        (default-keyword-arguments '(#:bar 3) '(#:foo 2))
        (default-keyword-arguments '(#:foo 2 #:bar 3) '(#:bar 6))))

(test-equal "substitute-keyword-arguments"
  '((#:foo 3)
    (#:foo 3)
    (#:foo 3 #:bar (1 2))
    (#:bar (1 2) #:foo 3)
    (#:foo 3))
  (list (substitute-keyword-arguments '(#:foo 2)
          ((#:foo f) (1+ f)))
        (substitute-keyword-arguments '()
          ((#:foo f 2) (1+ f)))
        (substitute-keyword-arguments '(#:foo 2 #:bar (2))
          ((#:foo f) (1+ f))
          ((#:bar b) (cons 1 b)))
        (substitute-keyword-arguments '(#:foo 2)
          ((#:foo _) 3)
          ((#:bar b '(2)) (cons 1 b)))
        (substitute-keyword-arguments '(#:foo 2)
          ((#:foo f 1) (1+ f))
          ((#:bar b) (cons 42 b)))))

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

(define (test-compression/decompression method run?)
  "Test METHOD, a symbol such as 'gzip.  Call RUN? to determine whether to
skip these tests."
  (unless (run?) (test-skip 1))
  (test-assert (format #f "compressed-port, decompressed-port, non-file [~a]"
                       method)
    (let ((data (call-with-input-file (search-path %load-path "guix.scm")
                  get-bytevector-all)))
      (let*-values (((compressed pids1)
                     (compressed-port method (open-bytevector-input-port data)))
                    ((decompressed pids2)
                     (decompressed-port method compressed)))
        (and (every (compose zero? cdr waitpid)
                    (pk 'pids method (append pids1 pids2)))
             (let ((result (get-bytevector-all decompressed)))
               (pk 'len method
                   (if (bytevector? result)
                       (bytevector-length result)
                       result)
                   (bytevector-length data))
               (equal? result data))))))

  (false-if-exception (delete-file temp-file))
  (unless (run?) (test-skip 1))
  (test-assert (format #f "compressed-output-port + decompressed-port [~a]"
                       method)
    (let* ((file (search-path %load-path "guix/derivations.scm"))
           (data (call-with-input-file file get-bytevector-all))
           (port (open-file temp-file "w0b")))
      (call-with-compressed-output-port method port
        (lambda (compressed)
          (put-bytevector compressed data)))
      (close-port port)

      (bytevector=? data
                    (call-with-decompressed-port method (open-file temp-file "r0b")
                      get-bytevector-all)))))

(for-each test-compression/decompression
          '(gzip xz lzip)
          (list (const #t) (const #t)))

;; This is actually in (guix store).
(test-equal "store-path-package-name"
  "bash-4.2-p24"
  (store-path-package-name
   (string-append (%store-prefix)
                  "/qvs2rj2ia5vci3wsdb7qvydrmacig4pg-bash-4.2-p24")))

(test-equal "canonical-newline-port"
  "This is a journey\nInto the sound\nA journey ...\n"
  (let ((port (open-string-input-port
               "This is a journey\r\nInto the sound\r\nA journey ...\n")))
    (get-string-all (canonical-newline-port port))))


(test-equal "edit-expression"
  "(display \"GNU Guix\")\n(newline)\n"
  (begin
    (call-with-output-file temp-file
      (lambda (port)
        (display "(display \"xiuG UNG\")\n(newline)\n" port)))
    (edit-expression `((filename . ,temp-file)
                       (line     . 0)
                       (column   . 9))
                     string-reverse)
    (call-with-input-file temp-file get-string-all)))

(test-end)

(false-if-exception (delete-file temp-file))
