;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-upstream)
  #:use-module (gnu packages base)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix import print)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix upstream)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))


(test-begin "upstream")

;; FIXME: Temporarily skipping this test; see <https://bugs.gnu.org/34229>.
(test-skip 1)

(test-equal "coalesce-sources same version"
  (list (upstream-source
         (package "foo") (version "1")
         (urls '("ftp://example.org/foo-1.tar.xz"
                 "ftp://example.org/foo-1.tar.gz"))
         (signature-urls '("ftp://example.org/foo-1.tar.xz.sig"
                           "ftp://example.org/foo-1.tar.gz.sig"))))

  (coalesce-sources (list (upstream-source
                           (package "foo") (version "1")
                           (urls '("ftp://example.org/foo-1.tar.gz"))
                           (signature-urls
                            '("ftp://example.org/foo-1.tar.gz.sig")))
                          (upstream-source
                           (package "foo") (version "1")
                           (urls '("ftp://example.org/foo-1.tar.xz"))
                           (signature-urls
                            '("ftp://example.org/foo-1.tar.xz.sig"))))))

(define test-package
  (package
    (name "test")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
    (build-system gnu-build-system)
    (inputs
     `(("hello" ,hello)))
    (native-inputs
     `(("sed" ,sed)
       ("tar" ,tar)))
    (propagated-inputs
     `(("grep" ,grep)))
    (home-page "http://localhost")
    (synopsis "test")
    (description "test")
    (license license:gpl3+)))

(define test-package-sexp
  '(package
    (name "test")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
    (build-system gnu-build-system)
    (inputs
     `(("hello" ,hello)))
    (native-inputs
     `(("sed" ,sed)
       ("tar" ,tar)))
    (propagated-inputs
     `(("grep" ,grep)))
    (home-page "http://localhost")
    (synopsis "test")
    (description "test")
    (license license:gpl3+)))

(test-equal "changed-inputs returns no changes"
  '()
  (changed-inputs test-package test-package-sexp))

(test-assert "changed-inputs returns changes to labelled input list"
  (let ((changes (changed-inputs
                  (package
                    (inherit test-package)
                    (inputs `(("hello" ,hello)
                              ("sed" ,sed))))
                  test-package-sexp)))
    (match changes
      ;; Exactly one change
      (((? upstream-input-change? item))
       (and (equal? (upstream-input-change-type item)
                    'regular)
            (equal? (upstream-input-change-action item)
                    'remove)
            (string=? (upstream-input-change-name item)
                      "sed")))
      (else (pk else #false)))))

(test-assert "changed-inputs returns changes to all labelled input lists"
  (let ((changes (changed-inputs
                  (package
                    (inherit test-package)
                    (inputs '())
                    (native-inputs '())
                    (propagated-inputs '()))
                  test-package-sexp)))
    (match changes
      (((? upstream-input-change? items) ...)
       (and (equal? (map upstream-input-change-type items)
                    '(regular native native propagated))
            (equal? (map upstream-input-change-action items)
                    '(add add add add))
            (equal? (map upstream-input-change-name items)
                    '("hello" "sed" "tar" "grep"))))
      (else (pk else #false)))))

(define test-new-package
  (package
    (inherit test-package)
    (inputs
     (list hello))
    (native-inputs
     (list sed tar))
    (propagated-inputs
     (list grep))))

(define test-new-package-sexp
  '(package
    (name "test")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
    (build-system gnu-build-system)
    (inputs
     (list hello))
    (native-inputs
     (list sed tar))
    (propagated-inputs
     (list grep))
    (home-page "http://localhost")
    (synopsis "test")
    (description "test")
    (license license:gpl3+)))

(test-assert "changed-inputs returns changes to plain input list"
  (let ((changes (changed-inputs
                  (package
                    (inherit test-new-package)
                    (inputs (list hello sed)))
                  test-new-package-sexp)))
    (match changes
      ;; Exactly one change
      (((? upstream-input-change? item))
       (and (equal? (upstream-input-change-type item)
                    'regular)
            (equal? (upstream-input-change-action item)
                    'remove)
            (string=? (upstream-input-change-name item)
                      "sed")))
      (else (pk else #false)))))

(test-assert "changed-inputs returns changes to all plain input lists"
  (let ((changes (changed-inputs
                  (package
                    (inherit test-new-package)
                    (inputs '())
                    (native-inputs '())
                    (propagated-inputs '()))
                  test-new-package-sexp)))
    (match changes
      (((? upstream-input-change? items) ...)
       (and (equal? (map upstream-input-change-type items)
                    '(regular native native propagated))
            (equal? (map upstream-input-change-action items)
                    '(add add add add))
            (equal? (map upstream-input-change-name items)
                    '("hello" "sed" "tar" "grep"))))
      (else (pk else #false)))))

(test-end)
