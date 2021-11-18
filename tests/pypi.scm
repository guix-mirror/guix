;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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
  #:use-module (guix memoization)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module (guix build-system python)
  #:use-module ((guix build utils) #:select (delete-file-recursively which mkdir-p))
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define test-json-1
  "{
  \"info\": {
    \"version\": \"1.0.0\",
    \"name\": \"foo\",
    \"license\": \"GNU LGPL\",
    \"summary\": \"summary\",
    \"home_page\": \"http://example.com\",
    \"classifiers\": [],
    \"download_url\": \"\"
  },
  \"urls\": [],
  \"releases\": {
    \"1.0.0\": [
      {
        \"url\": \"https://example.com/foo-1.0.0.egg\",
        \"packagetype\": \"bdist_egg\"
      }, {
        \"url\": \"https://example.com/foo-1.0.0.tar.gz\",
        \"packagetype\": \"sdist\"
      }, {
        \"url\": \"https://example.com/foo-1.0.0-py2.py3-none-any.whl\",
        \"packagetype\": \"bdist_wheel\"
      }
    ]
  }
}")

(define test-json-2
  "{
  \"info\": {
    \"version\": \"1.0.0\",
    \"name\": \"foo-99\",
    \"license\": \"GNU LGPL\",
    \"summary\": \"summary\",
    \"home_page\": \"http://example.com\",
    \"classifiers\": [],
    \"download_url\": \"\"
  },
  \"urls\": [],
  \"releases\": {
    \"1.0.0\": [
      {
        \"url\": \"https://example.com/foo-99-1.0.0.egg\",
        \"packagetype\": \"bdist_egg\"
      }, {
        \"url\": \"https://example.com/foo-99-1.0.0.tar.gz\",
        \"packagetype\": \"sdist\"
      }, {
        \"url\": \"https://example.com/foo-99-1.0.0-py2.py3-none-any.whl\",
        \"packagetype\": \"bdist_wheel\"
      }
    ]
  }
}")

(define test-source-hash
  "")

(define test-specifications
  '("Fizzy [foo, bar]"
    "PickyThing<1.6,>1.9,!=1.9.6,<2.0a0,==2.4c1"
    "SomethingWithMarker[foo]>1.0;python_version<\"2.7\""
    "requests [security,tests] >= 2.8.1, == 2.8.* ; python_version < \"2.7\""
    "pip @ https://github.com/pypa/pip/archive/1.3.1.zip#\
sha1=da9234ee9982d4bbb3c72346a6de940a148ea686"))

(define test-requires.txt "\
# A comment
 # A comment after a space
foo ~= 3
bar != 2

[test]
pytest (>=2.5.0)
")

;; Beaker contains only optional dependencies.
(define test-requires.txt-beaker "\
[crypto]
pycryptopp>=0.5.12

[cryptography]
cryptography

[testsuite]
Mock
coverage
")

(define test-metadata "\
Classifier: Programming Language :: Python :: 3.7
Requires-Dist: baz ~= 3
Requires-Dist: bar != 2
Provides-Extra: test
Requires-Dist: pytest (>=2.5.0) ; extra == 'test'
")

(define test-metadata-with-extras "
Classifier: Programming Language :: Python :: 3.7
Requires-Python: >=2.7, !=3.0.*, !=3.1.*, !=3.2.*, !=3.3.*
Requires-Dist: wrapt (<2,>=1)
Requires-Dist: bar

Provides-Extra: dev
Requires-Dist: tox ; extra == 'dev'
Requires-Dist: bumpversion (<1) ; extra == 'dev'
")

;;; Provides-Extra can appear before Requires-Dist.
(define test-metadata-with-extras-jedi "\
Requires-Python: >=2.7, !=3.0.*, !=3.1.*, !=3.2.*, !=3.3.*
Provides-Extra: testing
Requires-Dist: parso (>=0.3.0)
Provides-Extra: testing
Requires-Dist: pytest (>=3.1.0); extra == 'testing'
")


(test-begin "pypi")

(test-equal "guix-package->pypi-name, old URL style"
  "psutil"
  (guix-package->pypi-name
   (dummy-package "foo"
                  (source (dummy-origin
                           (uri
                            "https://pypi.org/packages/source/p/psutil/psutil-4.3.0.tar.gz"))))))

(test-equal "guix-package->pypi-name, new URL style"
  "certbot"
  (guix-package->pypi-name
   (dummy-package "foo"
                  (source (dummy-origin
                           (uri
                            "https://pypi.org/packages/a2/3b/4756e6a0ceb14e084042a2a65c615d68d25621c6fd446d0fc10d14c4ce7d/certbot-0.8.1.tar.gz"))))))

(test-equal "guix-package->pypi-name, several URLs"
  "cram"
  (guix-package->pypi-name
   (dummy-package "foo"
                  (source
                   (dummy-origin
                    (uri (list "https://bitheap.org/cram/cram-0.7.tar.gz"
                               (pypi-uri "cram" "0.7"))))))))

(test-equal "guix-package->pypi-name, honor 'upstream-name'"
  "bar-3"
  (guix-package->pypi-name
   (dummy-package "foo"
                  (properties
                   '((upstream-name . "bar-3"))))))

(test-equal "specification->requirement-name"
  '("Fizzy" "PickyThing" "SomethingWithMarker" "requests" "pip")
  (map specification->requirement-name test-specifications))

(test-equal "parse-requires.txt"
  (list '("foo" "bar") '("pytest"))
  (mock ((ice-9 ports) call-with-input-file
         call-with-input-string)
        (parse-requires.txt test-requires.txt)))

(test-equal "parse-requires.txt - Beaker"
  (list '() '("Mock" "coverage"))
  (mock ((ice-9 ports) call-with-input-file
         call-with-input-string)
        (parse-requires.txt test-requires.txt-beaker)))

(test-equal "parse-wheel-metadata, with extras"
  (list '("wrapt" "bar") '("tox" "bumpversion"))
  (mock ((ice-9 ports) call-with-input-file
         call-with-input-string)
        (parse-wheel-metadata test-metadata-with-extras)))

(test-equal "parse-wheel-metadata, with extras - Jedi"
  (list '("parso") '("pytest"))
  (mock ((ice-9 ports) call-with-input-file
         call-with-input-string)
        (parse-wheel-metadata test-metadata-with-extras-jedi)))

(test-assert "pypi->guix-package, no wheel"
  ;; Replace network resources with sample data.
    (mock ((guix import utils) url-fetch
           (lambda (url file-name)
             (match url
               ("https://example.com/foo-1.0.0.tar.gz"
                (begin
                  ;; Unusual requires.txt location should still be found.
                  (mkdir-p "foo-1.0.0/src/bizarre.egg-info")
                  (with-output-to-file "foo-1.0.0/src/bizarre.egg-info/requires.txt"
                    (lambda ()
                      (display test-requires.txt)))
                  (parameterize ((current-output-port (%make-void-port "rw+")))
                    (system* "tar" "czvf" file-name "foo-1.0.0/"))
                  (delete-file-recursively "foo-1.0.0")
                  (set! test-source-hash
                    (call-with-input-file file-name port-sha256))))
               ("https://example.com/foo-1.0.0-py2.py3-none-any.whl" #f)
               (_ (error "Unexpected URL: " url)))))
          (mock ((guix http-client) http-fetch
                 (lambda (url . rest)
                   (match url
                     ("https://pypi.org/pypi/foo/json"
                      (values (open-input-string test-json-1)
                              (string-length test-json-1)))
                     ("https://example.com/foo-1.0.0-py2.py3-none-any.whl" #f)
                     (_ (error "Unexpected URL: " url)))))
                (match (pypi->guix-package "foo")
                  (('package
                     ('name "python-foo")
                     ('version "1.0.0")
                     ('source ('origin
                                ('method 'url-fetch)
                                ('uri ('pypi-uri "foo" 'version))
                                ('sha256
                                 ('base32
                                  (? string? hash)))))
                     ('build-system 'python-build-system)
                     ('propagated-inputs ('list 'python-bar 'python-foo))
                     ('native-inputs ('list 'python-pytest))
                     ('home-page "http://example.com")
                     ('synopsis "summary")
                     ('description "summary")
                     ('license 'license:lgpl2.0))
                   (and (string=? (bytevector->nix-base32-string
                                   test-source-hash)
                                  hash)
                        (equal? (pypi->guix-package "foo" #:version "1.0.0")
                                (pypi->guix-package "foo"))
                        (catch 'quit
                          (lambda ()
                            (pypi->guix-package "foo" #:version "42"))
                          (const #t))))
                  (x
                   (pk 'fail x #f))))))

(test-skip (if (which "zip") 0 1))
(test-assert "pypi->guix-package, wheels"
  ;; Replace network resources with sample data.
  (mock ((guix import utils) url-fetch
         (lambda (url file-name)
           (match url
             ("https://example.com/foo-1.0.0.tar.gz"
              (begin
                (mkdir-p "foo-1.0.0/foo.egg-info/")
                (with-output-to-file "foo-1.0.0/foo.egg-info/requires.txt"
                  (lambda ()
                    (display "wrong data to make sure we're testing wheels ")))
                (parameterize ((current-output-port (%make-void-port "rw+")))
                  (system* "tar" "czvf" file-name "foo-1.0.0/"))
                (delete-file-recursively "foo-1.0.0")
                (set! test-source-hash
                  (call-with-input-file file-name port-sha256))))
             ("https://example.com/foo-1.0.0-py2.py3-none-any.whl"
              (begin
                (mkdir "foo-1.0.0.dist-info")
                (with-output-to-file "foo-1.0.0.dist-info/METADATA"
                  (lambda ()
                    (display test-metadata)))
                (let ((zip-file (string-append file-name ".zip")))
                  ;; zip always adds a "zip" extension to the file it creates,
                  ;; so we need to rename it.
                  (system* "zip" "-q" zip-file "foo-1.0.0.dist-info/METADATA")
                  (rename-file zip-file file-name))
                (delete-file-recursively "foo-1.0.0.dist-info")))
             (_ (error "Unexpected URL: " url)))))
        (mock ((guix http-client) http-fetch
               (lambda (url . rest)
                 (match url
                   ("https://pypi.org/pypi/foo/json"
                    (values (open-input-string test-json-1)
                            (string-length test-json-1)))
                   ("https://example.com/foo-1.0.0-py2.py3-none-any.whl" #f)
                   (_ (error "Unexpected URL: " url)))))
              ;; Not clearing the memoization cache here would mean returning the value
              ;; computed in the previous test.
              (invalidate-memoization! pypi->guix-package)
              (match (pypi->guix-package "foo")
                (('package
                   ('name "python-foo")
                   ('version "1.0.0")
                   ('source ('origin
                              ('method 'url-fetch)
                              ('uri ('pypi-uri "foo" 'version))
                              ('sha256
                               ('base32
                                (? string? hash)))))
                   ('build-system 'python-build-system)
                   ('propagated-inputs ('list 'python-bar 'python-baz))
                   ('native-inputs ('list 'python-pytest))
                   ('home-page "http://example.com")
                   ('synopsis "summary")
                   ('description "summary")
                   ('license 'license:lgpl2.0))
                 (string=? (bytevector->nix-base32-string
                            test-source-hash)
                           hash))
                (x
                 (pk 'fail x #f))))))

(test-assert "pypi->guix-package, no usable requirement file."
  ;; Replace network resources with sample data.
  (mock ((guix import utils) url-fetch
         (lambda (url file-name)
           (match url
             ("https://example.com/foo-1.0.0.tar.gz"
              (mkdir-p "foo-1.0.0/foo.egg-info/")
              (parameterize ((current-output-port (%make-void-port "rw+")))
                (system* "tar" "czvf" file-name "foo-1.0.0/"))
              (delete-file-recursively "foo-1.0.0")
              (set! test-source-hash
                (call-with-input-file file-name port-sha256)))
             ("https://example.com/foo-1.0.0-py2.py3-none-any.whl" #f)
             (_ (error "Unexpected URL: " url)))))
        (mock ((guix http-client) http-fetch
               (lambda (url . rest)
                 (match url
                   ("https://pypi.org/pypi/foo/json"
                    (values (open-input-string test-json-1)
                            (string-length test-json-1)))
                   ("https://example.com/foo-1.0.0-py2.py3-none-any.whl" #f)
                   (_ (error "Unexpected URL: " url)))))
              ;; Not clearing the memoization cache here would mean returning the value
              ;; computed in the previous test.
              (invalidate-memoization! pypi->guix-package)
              (match (pypi->guix-package "foo")
                (('package
                   ('name "python-foo")
                   ('version "1.0.0")
                   ('source ('origin
                              ('method 'url-fetch)
                              ('uri ('pypi-uri "foo" 'version))
                              ('sha256
                               ('base32
                                (? string? hash)))))
                   ('build-system 'python-build-system)
                   ('home-page "http://example.com")
                   ('synopsis "summary")
                   ('description "summary")
                   ('license 'license:lgpl2.0))
                 (string=? (bytevector->nix-base32-string
                            test-source-hash)
                           hash))
                (x
                 (pk 'fail x #f))))))

(test-assert "pypi->guix-package, package name contains \"-\" followed by digits"
  ;; Replace network resources with sample data.
  (mock ((guix import utils) url-fetch
         (lambda (url file-name)
           (match url
             ("https://example.com/foo-99-1.0.0.tar.gz"
              (begin
                ;; Unusual requires.txt location should still be found.
                (mkdir-p "foo-99-1.0.0/src/bizarre.egg-info")
                (with-output-to-file "foo-99-1.0.0/src/bizarre.egg-info/requires.txt"
                  (lambda ()
                    (display test-requires.txt)))
                (parameterize ((current-output-port (%make-void-port "rw+")))
                  (system* "tar" "czvf" file-name "foo-99-1.0.0/"))
                (delete-file-recursively "foo-99-1.0.0")
                (set! test-source-hash
                  (call-with-input-file file-name port-sha256))))
             ("https://example.com/foo-99-1.0.0-py2.py3-none-any.whl" #f)
             (_ (error "Unexpected URL: " url)))))
        (mock ((guix http-client) http-fetch
               (lambda (url . rest)
                 (match url
                   ("https://pypi.org/pypi/foo-99/json"
                    (values (open-input-string test-json-2)
                            (string-length test-json-2)))
                   ("https://example.com/foo-99-1.0.0-py2.py3-none-any.whl" #f)
                   (_ (error "Unexpected URL: " url)))))
              (match (pypi->guix-package "foo-99")
                (('package
                   ('name "python-foo-99")
                   ('version "1.0.0")
                   ('source ('origin
                              ('method 'url-fetch)
                              ('uri ('pypi-uri "foo-99" 'version))
                              ('sha256
                               ('base32
                                (? string? hash)))))
                   ('properties ('quote (("upstream-name" . "foo-99"))))
                   ('build-system 'python-build-system)
                   ('propagated-inputs ('list 'python-bar 'python-foo))
                   ('native-inputs ('list 'python-pytest))
                   ('home-page "http://example.com")
                   ('synopsis "summary")
                   ('description "summary")
                   ('license 'license:lgpl2.0))
                 (string=? (bytevector->nix-base32-string
                            test-source-hash)
                           hash))
                (x
                 (pk 'fail x #f))))))

(test-end "pypi")
