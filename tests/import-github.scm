;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (test-import-github)
  #:use-module (json)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64)
  #:use-module (guix git-download)
  #:use-module (guix http-client)
  #:use-module (guix import github)
  #:use-module (guix packages)
  #:use-module (guix tests)
  #:use-module (guix upstream)
  #:use-module (ice-9 match))

(test-begin "github")

(define (call-with-releases thunk tags releases)
  (mock ((guix http-client) http-fetch
         (lambda* (uri #:key headers)
           (unless (string-prefix? "mock://" uri)
             (error "the URI ~a should not be used" uri))
           (define components
             (string-split (substring uri 8) #\/))
           (pk 'stuff components headers)
           (define (scm->json-port scm)
             (open-input-string (scm->json-string scm)))
           (match components
             (("repos" "foo" "foomatics" "releases")
              (scm->json-port releases))
             (("repos" "foo" "foomatics" "tags")
              (scm->json-port tags))
             (rest (error "TODO ~a" rest)))))
        (parameterize ((%github-api "mock://"))
          (thunk))))

;; Copied from tests/minetest.scm
(define (upstream-source->sexp upstream-source)
  (define url (upstream-source-urls upstream-source))
  (unless (git-reference? url)
    (error "a <git-reference> is expected"))
  `(,(upstream-source-package upstream-source)
    ,(upstream-source-version upstream-source)
    ,(git-reference-url url)
    ,(git-reference-commit url)))

(define* (expected-sexp new-version new-commit)
  `("foomatics" ,new-version "https://github.com/foo/foomatics" ,new-commit))

(define (example-package old-version old-commit)
  (package
    (name "foomatics")
    (version old-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/foo/foomatics")
             (commit old-commit)))
       (sha256 #f) ; not important for following tests
       (file-name (git-file-name name version))))
    (build-system #f)
    (license #f)
    (synopsis #f)
    (description #f)
    (home-page #f)))

(define* (found-sexp old-version old-commit tags releases)
  (and=>
   (call-with-releases (lambda ()
                         ((upstream-updater-latest %github-updater)
                          (example-package old-version old-commit)))
                       tags releases)
   upstream-source->sexp))

(define-syntax-rule (test-release test-case old-version
                                  old-commit new-version new-commit
                                  tags releases)
  (test-equal test-case
    (expected-sexp new-version new-commit)
    (found-sexp old-version old-commit tags releases)))

(test-release "newest release is choosen"
  "1.0.0" "v1.0.0" "1.9" "v1.9"
  #()
  ;; a mixture of current, older and newer versions
  #((("tag_name" . "v0.0"))
    (("tag_name" . "v1.0.1"))
    (("tag_name" . "v1.9"))
    (("tag_name" . "v1.0.0"))
    (("tag_name" . "v1.0.2"))))

(test-release "tags are used when there are no formal releases"
  "1.0.0" "v1.0.0" "1.9" "v1.9"
  ;; a mixture of current, older and newer versions
  #((("name" . "v0.0"))
    (("name" . "v1.0.1"))
    (("name" . "v1.9"))
    (("name" . "v1.0.0"))
    (("name" . "v1.0.2")))
  #())

(test-release "\"version-\" prefixes are recognised"
  "1.0.0" "v1.0.0" "1.9" "version-1.9"
  #((("name" . "version-1.9")))
  #())

(test-release "prefixes are optional"
  "1.0.0" "v1.0.0" "1.9" "1.9"
  #((("name" . "1.9")))
  #())

(test-release "prefixing by package name is acceptable"
  "1.0.0" "v1.0.0" "1.9" "foomatics-1.9"
  #((("name" . "foomatics-1.9")))
  #())

(test-release "not all prefixes are acceptable"
  "1.0.0" "v1.0.0" "1.0.0" "v1.0.0"
  #((("name" . "v1.0.0"))
    (("name" . "barstatics-1.9")))
  #())

(test-end "github")
