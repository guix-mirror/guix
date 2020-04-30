;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-crate)
  #:use-module (guix import crate)
  #:use-module (guix base32)
  #:use-module (guix build-system cargo)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(define test-foo-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.0\",
    \"name\": \"foo\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\" \"test\"],
    \"categories\": [\"test\"]
    \"actual_versions\": [
      { \"id\": \"foo\",
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/foo/1.0.0/dependencies\"
        }
      }
    ]
  }
}")

(define test-foo-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"bar\",
       \"kind\": \"normal\"
     }
  ]
}")

(define test-root-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.0\",
    \"name\": \"root\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\" \"test\"],
    \"categories\": [\"test\"]
    \"actual_versions\": [
      { \"id\": \"foo\",
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/root/1.0.0/dependencies\"
        }
      }
    ]
  }
}")

(define test-root-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"intermediate-1\",
       \"kind\": \"normal\"
     },
     {
       \"crate_id\": \"intermediate-2\",
       \"kind\": \"normal\"
     }
     {
       \"crate_id\": \"leaf-alice\",
       \"kind\": \"normal\"
     },
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"normal\"
     }
  ]
}")

(define test-intermediate-1-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.0\",
    \"name\": \"intermediate-1\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\" \"test\"],
    \"categories\": [\"test\"]
    \"actual_versions\": [
      { \"id\": \"intermediate-1\",
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/intermediate-1/1.0.0/dependencies\"
        }
      }
    ]
  }
}")

(define test-intermediate-1-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"intermediate-2\",
       \"kind\": \"normal\"
     },
     {
       \"crate_id\": \"leaf-alice\",
       \"kind\": \"normal\"
     },
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"normal\"
     }
  ]
}")

(define test-intermediate-2-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.0\",
    \"name\": \"intermediate-2\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\" \"test\"],
    \"categories\": [\"test\"]
    \"actual_versions\": [
      { \"id\": \"intermediate-2\",
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/intermediate-2/1.0.0/dependencies\"
        }
      }
    ]
  }
}")

(define test-intermediate-2-dependencies
  "{
  \"dependencies\": [
     {
       \"crate_id\": \"leaf-bob\",
       \"kind\": \"normal\"
     }
  ]
}")

(define test-leaf-alice-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.0\",
    \"name\": \"leaf-alice\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\" \"test\"],
    \"categories\": [\"test\"]
    \"actual_versions\": [
      { \"id\": \"leaf-alice\",
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/leaf-alice/1.0.0/dependencies\"
        }
      }
    ]
  }
}")

(define test-leaf-alice-dependencies
  "{
  \"dependencies\": []
}")

(define test-leaf-bob-crate
  "{
  \"crate\": {
    \"max_version\": \"1.0.0\",
    \"name\": \"leaf-bob\",
    \"description\": \"summary\",
    \"homepage\": \"http://example.com\",
    \"repository\": \"http://example.com\",
    \"keywords\": [\"dummy\" \"test\"],
    \"categories\": [\"test\"]
    \"actual_versions\": [
      { \"id\": \"leaf-bob\",
        \"num\": \"1.0.0\",
        \"license\": \"MIT OR Apache-2.0\",
        \"links\": {
          \"dependencies\": \"/api/v1/crates/leaf-bob/1.0.0/dependencies\"
        }
      }
    ]
  }
}")

(define test-leaf-bob-dependencies
  "{
  \"dependencies\": []
}")

(define test-source-hash
  "")


(test-begin "crate")

(test-equal "guix-package->crate-name"
  "rustc-serialize"
  (guix-package->crate-name
   (dummy-package
    "rust-rustc-serialize"
    (source (dummy-origin
     (uri (crate-uri "rustc-serialize" "1.0")))))))

(test-assert "crate->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://crates.io/api/v1/crates/foo"
              (open-input-string test-foo-crate))
             ("https://crates.io/api/v1/crates/foo/1.0.0/download"
              (set! test-source-hash
                (bytevector->nix-base32-string
                 (sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/foo/1.0.0/dependencies"
              (open-input-string test-foo-dependencies))
             (_ (error "Unexpected URL: " url)))))
    (match (crate->guix-package "foo")
      (('package
         ('name "rust-foo")
         ('version "1.0.0")
         ('source ('origin
                    ('method 'url-fetch)
                    ('uri ('crate-uri "foo" 'version))
                    ('file-name ('string-append 'name "-" 'version ".tar.gz"))
                    ('sha256
                     ('base32
                      (? string? hash)))))
         ('build-system 'cargo-build-system)
         ('arguments
          ('quasiquote
           ('#:cargo-inputs (("rust-bar" ('unquote rust-bar))))))
         ('home-page "http://example.com")
         ('synopsis "summary")
         ('description "summary")
         ('license ('list 'license:expat 'license:asl2.0)))
       (string=? test-source-hash hash))
      (x
       (pk 'fail x #f)))))

(test-assert "cargo-recursive-import"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://crates.io/api/v1/crates/root"
              (open-input-string test-root-crate))
             ("https://crates.io/api/v1/crates/root/1.0.0/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/root/1.0.0/dependencies"
              (open-input-string test-root-dependencies))
             ("https://crates.io/api/v1/crates/intermediate-1"
              (open-input-string test-intermediate-1-crate))
             ("https://crates.io/api/v1/crates/intermediate-1/1.0.0/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/intermediate-1/1.0.0/dependencies"
              (open-input-string test-intermediate-1-dependencies))
             ("https://crates.io/api/v1/crates/intermediate-2"
              (open-input-string test-intermediate-2-crate))
             ("https://crates.io/api/v1/crates/intermediate-2/1.0.0/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/intermediate-2/1.0.0/dependencies"
              (open-input-string test-intermediate-2-dependencies))
             ("https://crates.io/api/v1/crates/leaf-alice"
              (open-input-string test-leaf-alice-crate))
             ("https://crates.io/api/v1/crates/leaf-alice/1.0.0/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/leaf-alice/1.0.0/dependencies"
              (open-input-string test-leaf-alice-dependencies))
             ("https://crates.io/api/v1/crates/leaf-bob"
              (open-input-string test-leaf-bob-crate))
             ("https://crates.io/api/v1/crates/leaf-bob/1.0.0/download"
              (set! test-source-hash
                    (bytevector->nix-base32-string
                     (sha256 (string->bytevector "empty file\n" "utf-8"))))
              (open-input-string "empty file\n"))
             ("https://crates.io/api/v1/crates/leaf-bob/1.0.0/dependencies"
              (open-input-string test-leaf-bob-dependencies))
             (_ (error "Unexpected URL: " url)))))
        (match (crate-recursive-import "root")
          ;; rust-intermediate-2 has no dependency on the rust-leaf-alice package, so this is a valid ordering
          ((('package
              ('name "rust-leaf-alice")
              ('version (? string? ver))
              ('source
               ('origin
                 ('method 'url-fetch)
                 ('uri ('crate-uri "leaf-alice" 'version))
                 ('file-name
                  ('string-append 'name "-" 'version ".tar.gz"))
                 ('sha256
                  ('base32
                   (? string? hash)))))
              ('build-system 'cargo-build-system)
              ('home-page "http://example.com")
              ('synopsis "summary")
              ('description "summary")
              ('license ('list 'license:expat 'license:asl2.0)))
            ('package
              ('name "rust-leaf-bob")
              ('version (? string? ver))
              ('source
               ('origin
                 ('method 'url-fetch)
                 ('uri ('crate-uri "leaf-bob" 'version))
                 ('file-name
                  ('string-append 'name "-" 'version ".tar.gz"))
                 ('sha256
                  ('base32
                   (? string? hash)))))
              ('build-system 'cargo-build-system)
              ('home-page "http://example.com")
              ('synopsis "summary")
              ('description "summary")
              ('license ('list 'license:expat 'license:asl2.0)))
            ('package
              ('name "rust-intermediate-2")
              ('version (? string? ver))
              ('source
               ('origin
                 ('method 'url-fetch)
                 ('uri ('crate-uri "intermediate-2" 'version))
                 ('file-name
                  ('string-append 'name "-" 'version ".tar.gz"))
                 ('sha256
                  ('base32
                   (? string? hash)))))
              ('build-system 'cargo-build-system)
              ('arguments
               ('quasiquote
                ('#:cargo-inputs (("rust-leaf-bob" ('unquote rust-leaf-bob))))))
              ('home-page "http://example.com")
              ('synopsis "summary")
              ('description "summary")
              ('license ('list 'license:expat 'license:asl2.0)))
            ('package
              ('name "rust-intermediate-1")
              ('version (? string? ver))
              ('source
               ('origin
                 ('method 'url-fetch)
                 ('uri ('crate-uri "intermediate-1" 'version))
                 ('file-name
                  ('string-append 'name "-" 'version ".tar.gz"))
                 ('sha256
                  ('base32
                   (? string? hash)))))
              ('build-system 'cargo-build-system)
              ('arguments
               ('quasiquote
                ('#:cargo-inputs (("rust-intermediate-2" ('unquote rust-intermediate-2))
                                  ("rust-leaf-alice" ('unquote rust-leaf-alice))
                                  ("rust-leaf-bob" ('unquote rust-leaf-bob))))))
              ('home-page "http://example.com")
              ('synopsis "summary")
              ('description "summary")
              ('license ('list 'license:expat 'license:asl2.0)))
            ('package
              ('name "rust-root")
              ('version (? string? ver))
              ('source
               ('origin
                 ('method 'url-fetch)
                 ('uri ('crate-uri "root" 'version))
                 ('file-name
                  ('string-append 'name "-" 'version ".tar.gz"))
                 ('sha256
                  ('base32
                   (? string? hash)))))
              ('build-system 'cargo-build-system)
              ('arguments
               ('quasiquote
                ('#:cargo-inputs (("rust-intermediate-1" ('unquote rust-intermediate-1))
                                  ("rust-intermediate-2" ('unquote rust-intermediate-2))
                                  ("rust-leaf-alice" ('unquote rust-leaf-alice))
                                  ("rust-leaf-bob" ('unquote rust-leaf-bob))))))
              ('home-page "http://example.com")
              ('synopsis "summary")
              ('description "summary")
              ('license ('list 'license:expat 'license:asl2.0))))
           #t)
          (x
           (pk 'fail x #f)))))

(test-equal "licenses: MIT OR Apache-2.0"
  '(license:expat license:asl2.0)
  (string->license "MIT OR Apache-2.0"))

(test-equal "licenses: Apache-2.0 / MIT"
  '(license:asl2.0 license:expat)
  (string->license "Apache-2.0 / MIT"))

(test-equal "licenses: Apache-2.0 WITH LLVM-exception"
  '(license:asl2.0 unknown-license!)
  (string->license "Apache-2.0 WITH LLVM-exception"))

(test-equal "licenses: MIT/Apache-2.0 AND BSD-2-Clause"
  '(license:expat license:asl2.0 unknown-license!)
  (string->license "MIT/Apache-2.0 AND BSD-2-Clause"))

(test-equal "licenses: MIT/Apache-2.0"
  '(license:expat license:asl2.0)
  (string->license "MIT/Apache-2.0"))

(test-end "crate")
