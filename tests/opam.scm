;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module ((guix build utils) #:select (delete-file-recursively mkdir-p which))
  #:use-module (srfi srfi-64)
  #:use-module (web uri)
  #:use-module (ice-9 match))

(define test-url-file
  "http: \"https://example.org/foo-1.0.0.tar.gz\"
checksum: \"ac8920f39a8100b94820659bc2c20817\"")

(define test-source-hash
  "")

(define test-urls
  "repo ac8920f39a8100b94820659bc2c20817 0o644
packages/foo/foo.1.0.0/url ac8920f39a8100b94820659bc2c20817 0o644
packages/foo/foo.1.0.0/opam ac8920f39a8100b94820659bc2c20817 0o644
packages/foo/foo.1.0.0/descr ac8920f39a8100b94820659bc2c20817 0o644")

(define test-opam-file
"opam-version: 1.2
maintainer: \"Alice Doe\"
authors: \"Alice Doe, John Doe\"
homepage: \"https://example.org/\"
bug-reports: \"https://example.org/bugs\"
license: \"MIT\"
dev-repo: \"https://example.org/git\"
build: [
  \"ocaml\" \"pkg/pkg.ml\" \"build\" \"--pinned\" \"%{pinned}%\"
]
build-test: [
  \"ocaml\" \"pkg/pkg.ml\" \"build\" \"--pinned\" \"%{pinned}%\" \"--tests\" \"true\"
]
depends: [
  \"alcotest\" {test & >= \"0.7.2\"}
  \"ocamlbuild\" {build & >= \"0.9.2\"}
]")

(test-begin "opam")

(test-assert "opam->guix-package"
  ;; Replace network resources with sample data.
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
          (mock ((guix http-client) http-fetch/cached
                 (lambda (url . rest)
                   (match (uri->string url)
                     ("https://opam.ocaml.org/urls.txt"
                      (values (open-input-string test-urls)
                              (string-length test-urls)))
                     (_ (error "Unexpected URL: " url)))))
                (mock ((guix http-client) http-fetch
                       (lambda (url . rest)
                         (match url
                           ("https://opam.ocaml.org/packages/foo/foo.1.0.0/url"
                            (values (open-input-string test-url-file)
                                    (string-length test-url-file)))
                           ("https://opam.ocaml.org/packages/foo/foo.1.0.0/opam"
                            (values (open-input-string test-opam-file)
                                    (string-length test-opam-file)))
                           (_ (error "Unexpected URL: " url)))))
                      (match (opam->guix-package "foo")
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
                           ('inputs
                            ('quasiquote
                             (("ocamlbuild" ('unquote 'ocamlbuild))
                              ("ocaml-alcotest" ('unquote 'ocaml-alcotest)))))
                           ('home-page "https://example.org/")
                           ('synopsis "")
                           ('description "")
                           ('license 'license:expat))
                         (string=? (bytevector->nix-base32-string
                                    test-source-hash)
                                   hash))
                        (x
                         (pk 'fail x #f)))))))

(test-end "opam")
