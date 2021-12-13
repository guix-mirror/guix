;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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

(define-module (test-opam)
  #:use-module (guix import opam)
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module ((guix build utils) #:select (delete-file-recursively mkdir-p which))
  #:use-module ((guix utils) #:select (call-with-temporary-output-file))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg))

(define test-opam-file
"opam-version: \"2.0\"
  version: \"1.0.0\"
maintainer: \"Alice Doe\"
authors: [
  \"Alice Doe\"
  \"John Doe\"
]
homepage: \"https://example.org/\"
bug-reports: \"https://example.org/bugs\"
dev-repo: \"https://example.org/git\"
build: [
  [\"ocaml\" \"pkg/pkg.ml\" \"build\" \"--pinned\" \"%{pinned}%\"]
]
build-test: [
  [\"ocaml\" \"pkg/pkg.ml\" \"build\" \"--pinned\" \"%{pinned}%\" \"--tests\" \"true\"]
]
depends: [
  \"alcotest\" {test & >= \"0.7.2\"}
  \"ocamlbuild\" {build & >= \"0.9.2\"}
  \"zarith\" {>= \"0.7\"}
]
synopsis: \"Some example package\"
description: \"\"\"
This package is just an example.\"\"\"
license: \"BSD-3-Clause\"
url {
  src: \"https://example.org/foo-1.0.0.tar.gz\"
  checksum: \"md5=74c6e897658e820006106f45f736381f\"
}")

(define test-source-hash
  "")

(define test-repo
  (mkdtemp! "/tmp/opam-repo.XXXXXX"))

(test-begin "opam")

(test-assert "opam->guix-package"
  (mock ((guix import opam) get-opam-repository
         (const test-repo))
        (mock ((guix import utils) url-fetch
               (lambda (url file-name)
                 (match url
                   ("https://example.org/foo-1.0.0.tar.gz"
                    (begin
                      (mkdir-p "foo-1.0.0")
                      (system* "tar" "czvf" file-name "foo-1.0.0/")
                      (delete-file-recursively "foo-1.0.0")
                      (set! test-source-hash
                            (call-with-input-file file-name port-sha256))))
                   (_ (error "Unexpected URL: " url)))))
              (let ((my-package (string-append test-repo
                                               "/packages/foo/foo.1.0.0")))
                (mkdir-p my-package)
                (with-output-to-file (string-append my-package "/opam")
                  (lambda _
                    (format #t "~a" test-opam-file))))
              (match (opam->guix-package "foo" #:repo (list test-repo))
                (('package
                   ('name "ocaml-foo")
                   ('version "1.0.0")
                   ('source ('origin
                              ('method 'url-fetch)
                              ('uri "https://example.org/foo-1.0.0.tar.gz")
                              ('sha256
                               ('base32
                                (? string? hash)))))
                   ('build-system 'ocaml-build-system)
                   ('propagated-inputs ('list 'ocaml-zarith))
                   ('native-inputs
                    ('list 'ocaml-alcotest 'ocamlbuild))
                   ('home-page "https://example.org/")
                   ('synopsis "Some example package")
                   ('description "This package is just an example.")
                   ('license 'license:bsd-3))
                 (string=? (bytevector->nix-base32-string
                            test-source-hash)
                           hash))
                (x
                 (pk 'fail x #f))))))

;; Test the opam file parser
;; We fold over some test cases. Each case is a pair of the string to parse and the
;; expected result.
(define (test-opam-syntax name pattern test-cases)
  (test-assert name
    (fold (lambda (test acc)
            (display test) (newline)
            (match test
              ((str . expected)
               (and acc
                    (let ((result (peg:tree (match-pattern pattern str))))
                      (if (equal? result expected)
                          #t
                          (pk 'fail (list str result expected) #f)))))))
          #t test-cases)))

(test-opam-syntax
  "parse-strings" string-pat
  '(("" . #f)
    ("\"hello\"" . (string-pat "hello"))
    ("\"hello world\"" . (string-pat "hello world"))
    ("\"The dreaded \\\"é\\\"\"" . (string-pat "The dreaded \"é\""))
    ("\"Have some \\\\\\\\ :)\"" . (string-pat "Have some \\\\ :)"))
    ("\"今日は\"" . (string-pat "今日は"))))

(test-opam-syntax
  "parse-multiline-strings" multiline-string
  '(("" . #f)
    ("\"\"\"hello\"\"\"" . (multiline-string "hello"))
    ("\"\"\"hello \"world\"!\"\"\"" . (multiline-string "hello \"world\"!"))
    ("\"\"\"hello \"\"world\"\"!\"\"\"" . (multiline-string "hello \"\"world\"\"!"))))

(test-opam-syntax
  "parse-lists" list-pat
  '(("" . #f)
    ("[]" . list-pat)
    ("[make]" . (list-pat (var "make")))
    ("[\"make\"]" . (list-pat (string-pat "make")))
    ("[\n  a\n  b\n  c]" . (list-pat (var "a") (var "b") (var "c")))
    ("[a   b     \"c\"]" . (list-pat (var "a") (var "b") (string-pat "c")))
    ;; complex lists
    ("[(a & b)]" . (list-pat (choice-pat (group-pat (var "a") (var "b")))))
    ("[(a | b & c)]" . (list-pat (choice-pat (var "a") (group-pat (var "b") (var "c")))))
    ("[a (b | c) d]" . (list-pat (var "a") (choice-pat (var "b") (var "c")) (var "d")))))

(test-opam-syntax
  "parse-dicts" dict
  '(("" . #f)
    ("{}" . dict)
    ("{a: \"b\"}" . (dict (record "a" (string-pat "b"))))
    ("{a: \"b\"\nc: \"d\"}" . (dict (record "a" (string-pat "b")) (record "c" (string-pat "d"))))))

(test-opam-syntax
  "parse-conditions" condition
  '(("" . #f)
    ("{}" . #f)
    ("{build}" . (condition-var "build"))
    ("{>= \"0.2.0\"}" . (condition-greater-or-equal
                          (condition-string "0.2.0")))
    ("{>= \"0.2.0\" & test}" . (condition-and
                                 (condition-greater-or-equal
                                   (condition-string "0.2.0"))
                                 (condition-var "test")))
    ("{>= \"0.2.0\" | build}" . (condition-or
                                 (condition-greater-or-equal
                                   (condition-string "0.2.0"))
                                 (condition-var "build")))
    ("{ = \"1.0+beta19\" }" . (condition-eq
                                (condition-string "1.0+beta19")))))

(test-opam-syntax
  "parse-comment" list-pat
  '(("" . #f)
    ("[#comment\n]" . list-pat)))

(test-end "opam")
