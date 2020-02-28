;;; GNU Guix --- Functional package management for GNU
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

(define-module (test-swh)
  #:use-module (guix swh)
  #:use-module (guix tests http)
  #:use-module (web response)
  #:use-module (srfi srfi-64))

;; Test the JSON mapping machinery used in (guix swh).

(define %origin
  "{ \"visits_url\": \"/visits/42\",
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
  (with-http-server `((200 ,str))
    (parameterize ((%swh-base-url (%local-url)))
      exp ...)))

(test-begin "swh")

(test-equal "lookup-origin"
  (list "git" "http://example.org/guix.git")
  (with-json-result %origin
    (let ((origin (lookup-origin "http://example.org/guix.git")))
      (list (origin-type origin)
            (origin-url origin)))))

(test-equal "lookup-origin, not found"
  #f
  (with-http-server `((404 "Nope."))
    (parameterize ((%swh-base-url (%local-url)))
      (lookup-origin "http://example.org/whatever"))))

(test-equal "lookup-directory"
  '(("one" 123) ("two" 456))
  (with-json-result %directory-entries
    (map (lambda (entry)
           (list (directory-entry-name entry)
                 (directory-entry-length entry)))
         (lookup-directory "123"))))

(test-equal "rate limit reached"
  3000000000
  (let ((too-many (build-response
                   #:code 429
                   #:reason-phrase "Too many requests"

                   ;; Pretend we've reached the limit and it'll be reset in
                   ;; June 2065.
                   #:headers '((x-ratelimit-remaining . "0")
                               (x-ratelimit-reset . "3000000000")))))
    (with-http-server `((,too-many "Too bad."))
      (parameterize ((%swh-base-url (%local-url)))
        (catch 'swh-error
          (lambda ()
            (lookup-origin "http://example.org/guix.git"))
          (lambda (key url method response)
            ;; Ensure the reset time was recorded.
            (@@ (guix swh) %general-rate-limit-reset-time)))))))

(test-assert "%allow-request? and request-rate-limit-reached?"
  ;; Here we test two things: that the rate limit set above is in effect and
  ;; that %ALLOW-REQUEST? is called, and that 'request-rate-limit-reached?'
  ;; returns true.
  (let* ((key (gensym "skip-request"))
         (skip-if-limit-reached
          (lambda (url method)
            (or (not (request-rate-limit-reached? url method))
                (throw key #t)))))
    (parameterize ((%allow-request? skip-if-limit-reached))
      (catch key
        (lambda ()
          (lookup-origin "http://example.org/guix.git")
          #f)
        (const #t)))))

(test-end "swh")

;; Local Variables:
;; eval: (put 'with-json-result 'scheme-indent-function 1)
;; eval: (put 'with-http-server 'scheme-indent-function 1)
;; End:

