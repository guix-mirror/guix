;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-swh)
  #:use-module (guix swh)
  #:use-module (guix tests http)
  #:use-module (srfi srfi-64))

;; Test the JSON mapping machinery used in (guix swh).

(define %origin
  "{ \"id\": 42,
     \"visits_url\": \"/visits/42\",
     \"type\": \"git\",
     \"url\": \"http://example.org/guix.git\" }")

(define %directory-entries
  "[ { \"name\": \"one\",
       \"type\": \"regular\",
       \"length\": 123,
       \"dir_id\": 1 }
     { \"name\": \"two\",
       \"type\": \"regular\",
       \"length\": 456,
       \"dir_id\": 2 } ]")

(define-syntax-rule (with-json-result str exp ...)
  (with-http-server 200 str
    (parameterize ((%swh-base-url (%local-url)))
      exp ...)))

(test-begin "swh")

(test-equal "lookup-origin"
  (list 42 "git" "http://example.org/guix.git")
  (with-json-result %origin
    (let ((origin (lookup-origin "http://example.org/guix.git")))
      (list (origin-id origin)
            (origin-type origin)
            (origin-url origin)))))

(test-equal "lookup-origin, not found"
  #f
  (with-http-server 404 "Nope."
    (parameterize ((%swh-base-url (%local-url)))
      (lookup-origin "http://example.org/whatever"))))

(test-equal "lookup-directory"
  '(("one" 123) ("two" 456))
  (with-json-result %directory-entries
    (map (lambda (entry)
           (list (directory-entry-name entry)
                 (directory-entry-length entry)))
         (lookup-directory "123"))))

(test-end "swh")

;; Local Variables:
;; eval: (put 'with-json-result 'scheme-indent-function 1)
;; End:

