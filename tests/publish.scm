;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

;; Avoid interference.
(unsetenv "http_proxy")

(define-module (test-publish)
  #:use-module (guix scripts publish)
  #:use-module (guix tests)
  #:use-module (guix config)
  #:use-module (guix utils)
  #:use-module (guix hash)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module (guix base64)
  #:use-module ((guix serialization) #:select (restore-file))
  #:use-module (guix pk-crypto)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim))

(define %store
  (open-connection-for-tests))

(define %reference (add-text-to-store %store "ref" "foo"))

(define %item (add-text-to-store %store "item" "bar" (list %reference)))

(define (http-get-body uri)
  (call-with-values (lambda () (http-get uri))
    (lambda (response body) body)))

(define (publish-uri route)
  (string-append "http://localhost:6789" route))

;; Run a local publishing server in a separate thread.
(call-with-new-thread
 (lambda ()
   (guix-publish "--port=6789"))) ; attempt to avoid port collision

;; Wait until the server is accepting connections.
(let ((conn (socket PF_INET SOCK_STREAM 0)))
  (let loop ()
    (unless (false-if-exception
             (connect conn AF_INET (inet-pton AF_INET "127.0.0.1") 6789))
      (loop))))


(test-begin "publish")

(test-equal "/nix-cache-info"
  (format #f "StoreDir: ~a\nWantMassQuery: 0\nPriority: 100\n"
          %store-directory)
  (http-get-body (publish-uri "/nix-cache-info")))

(test-equal "/*.narinfo"
  (let* ((info (query-path-info %store %item))
         (unsigned-info
          (format #f
                  "StorePath: ~a
URL: nar/~a
Compression: none
NarHash: sha256:~a
NarSize: ~d
References: ~a~%"
                  %item
                  (basename %item)
                  (bytevector->nix-base32-string
                   (path-info-hash info))
                  (path-info-nar-size info)
                  (basename (first (path-info-references info)))))
         (signature (base64-encode
                     (string->utf8
                      (canonical-sexp->string
                       ((@@ (guix scripts publish) signed-string)
                        unsigned-info))))))
    (format #f "~aSignature: 1;~a;~a~%"
            unsigned-info (gethostname) signature))
  (utf8->string
   (http-get-body
    (publish-uri
     (string-append "/" (store-path-hash-part %item) ".narinfo")))))

(test-equal "/nar/*"
  "bar"
  (call-with-temporary-output-file
   (lambda (temp port)
     (let ((nar (utf8->string
                 (http-get-body
                  (publish-uri
                   (string-append "/nar/" (basename %item)))))))
       (call-with-input-string nar (cut restore-file <> temp)))
     (call-with-input-file temp read-string))))

(test-equal "/nar/invalid"
  404
  (begin
    (call-with-output-file (string-append (%store-prefix) "/invalid")
      (lambda (port)
        (display "This file is not a valid store item." port)))
    (response-code (http-get (publish-uri (string-append "/nar/invalid"))))))

(test-end "publish")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
