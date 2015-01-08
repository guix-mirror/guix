;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

(define-module (test-pypi)
  #:use-module (guix import pypi)
  #:use-module (guix base32)
  #:use-module (guix hash)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define test-json
  "{
  \"info\": {
    \"version\": \"1.0.0\",
    \"name\": \"foo\",
    \"license\": \"GNU LGPL\",
    \"summary\": \"summary\",
    \"home_page\": \"http://example.com\",
  },
  \"releases\": {
    \"1.0.0\": [
      {
        \"url\": \"https://example.com/foo-1.0.0.egg\",
        \"packagetype\": \"bdist_egg\",
      }, {
        \"url\": \"https://example.com/foo-1.0.0.tar.gz\",
        \"packagetype\": \"sdist\",
      }
    ]
  }
}")

(define test-source
  "foobar")

(test-begin "pypi")

(test-assert "pypi->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix import utils) url-fetch
         (lambda (url file-name)
           (with-output-to-file file-name
             (lambda ()
               (display
                (match url
                  ("https://pypi.python.org/pypi/foo/json"
                   test-json)
                  ("https://example.com/foo-1.0.0.tar.gz"
                   test-source)
                  (_ (error "Unexpected URL: " url))))))))
    (match (pypi->guix-package "foo")
      (('package
         ('name "python-foo")
         ('version "1.0.0")
         ('source ('origin
                    ('method 'url-fetch)
                    ('uri ('string-append "https://example.com/foo-"
                                          'version ".tar.gz"))
                    ('sha256
                     ('base32
                      (? string? hash)))))
         ('build-system 'python-build-system)
         ('inputs
          ('quasiquote
           (("python-setuptools" ('unquote 'python-setuptools)))))
         ('home-page "http://example.com")
         ('synopsis "summary")
         ('description "summary")
         ('license 'lgpl2.0))
       (string=? (bytevector->nix-base32-string
                  (call-with-input-string test-source port-sha256))
                 hash))
      (x
       (pk 'fail x #f)))))

(test-end "pypi")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
