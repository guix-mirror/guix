;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Cyril Roelandt <tipecaml@gmail.com>
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


(define-module (test-packages)
  #:use-module (guix build download)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix scripts lint)
  #:use-module (guix ui)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (srfi srfi-64))

;; Test the linter.


(test-begin "lint")

(define-syntax-rule (dummy-package name* extra-fields ...)
  (package extra-fields ... (name name*) (version "0") (source #f)
           (build-system gnu-build-system)
           (synopsis #f) (description #f)
           (home-page #f) (license #f) ))

(define (call-with-warnings thunk)
  (let ((port (open-output-string)))
    (parameterize ((guix-warning-port port))
      (thunk))
    (get-output-string port)))

(test-assert "description: does not start with an upper-case letter"
  (->bool
   (string-contains (call-with-warnings
                      (lambda ()
                        (let ((pkg (dummy-package "x"
                                     (description "bad description."))))
                          (check-description-style pkg))))
                    "description should start with an upper-case letter")))

(test-assert "synopsis: does not start with an upper-case letter"
  (->bool
   (string-contains (call-with-warnings
                      (lambda ()
                        (let ((pkg (dummy-package "x"
                                     (synopsis "bad synopsis."))))
                          (check-synopsis-style pkg))))
                    "synopsis should start with an upper-case letter")))

(test-assert "synopsis: ends with a period"
  (->bool
   (string-contains (call-with-warnings
                      (lambda ()
                        (let ((pkg (dummy-package "x"
                                     (synopsis "Bad synopsis."))))
                          (check-synopsis-style pkg))))
                    "no period allowed at the end of the synopsis")))

(test-assert "synopsis: ends with 'etc.'"
  (->bool
   (string-null? (call-with-warnings
                   (lambda ()
                     (let ((pkg (dummy-package "x"
                                  (synopsis "Foo, bar, etc."))))
                       (check-synopsis-style pkg)))))))

(test-assert "synopsis: starts with 'A'"
  (->bool
   (string-contains (call-with-warnings
                      (lambda ()
                        (let ((pkg (dummy-package "x"
                                     (synopsis "A bad synopŝis"))))
                          (check-synopsis-style pkg))))
                    "no article allowed at the beginning of the synopsis")))

(test-assert "synopsis: starts with 'An'"
  (->bool
   (string-contains (call-with-warnings
                      (lambda ()
                        (let ((pkg (dummy-package "x"
                                     (synopsis "An awful synopsis"))))
                        (check-synopsis-style pkg))))
                    "no article allowed at the beginning of the synopsis")))

(test-assert "synopsis: too long"
  (->bool
   (string-contains (call-with-warnings
                      (lambda ()
                        (let ((pkg (dummy-package "x"
                                     (synopsis (make-string 80 #\x)))))
                          (check-synopsis-style pkg))))
                    "synopsis should be less than 80 characters long")))

(test-assert "inputs: pkg-config is probably a native input"
  (->bool
   (string-contains
     (call-with-warnings
       (lambda ()
         (let ((pkg (dummy-package "x"
                      (inputs `(("pkg-config" ,pkg-config))))))
              (check-inputs-should-be-native pkg))))
         "pkg-config should probably be a native input")))

(test-assert "patches: file names"
  (->bool
   (string-contains
     (call-with-warnings
       (lambda ()
         (let ((pkg (dummy-package "x"
                      (source
                       (origin
                        (method url-fetch)
                        (uri "someurl")
                        (sha256 "somesha")
                        (patches (list "/path/to/y.patch")))))))
              (check-patches pkg))))
         "file names of patches should start with the package name")))

(test-end "lint")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
