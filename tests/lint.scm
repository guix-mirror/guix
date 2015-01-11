;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix tests)
  #:use-module (guix build download)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix scripts lint)
  #:use-module (guix ui)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (web server)
  #:use-module (web server http)
  #:use-module (web response)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-64))

;; Test the linter.

(define %http-server-port
  ;; TCP port to use for the stub HTTP server.
  9999)

(define %local-url
  ;; URL to use for 'home-page' tests.
  (string-append "http://localhost:" (number->string %http-server-port)
                 "/foo/bar"))

(define %http-server-socket
  ;; Socket used by the Web server.
  (catch 'system-error
    (lambda ()
      (let ((sock (socket PF_INET SOCK_STREAM 0)))
        (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
        (bind sock
              (make-socket-address AF_INET INADDR_LOOPBACK
                                   %http-server-port))
        sock))
    (lambda args
      (let ((err (system-error-errno args)))
        (format (current-error-port)
                "warning: cannot run Web server for tests: ~a~%"
                (strerror err))
        #f))))

(define (http-write server client response body)
  "Write RESPONSE."
  (let* ((response (write-response response client))
         (port     (response-port response)))
    (cond
     ((not body))                                 ;pass
     (else
      (write-response-body response body)))
    (close-port port)
    (quit #t)                                     ;exit the server thread
    (values)))

(define-server-impl stub-http-server
  ;; Stripped-down version of Guile's built-in HTTP server.
  (@@ (web server http) http-open)
  (@@ (web server http) http-read)
  http-write
  (@@ (web server http) http-close))

(define (call-with-http-server code thunk)
  "Call THUNK with an HTTP server running and returning CODE on HTTP
requests."
  (define (server-body)
    (define (handle request body)
      (values (build-response #:code code
                              #:reason-phrase "Such is life")
              "Hello, world."))

    (catch 'quit
      (lambda ()
        (run-server handle stub-http-server
                    `(#:socket ,%http-server-socket)))
      (const #t)))

  (let* ((server (make-thread server-body)))
    ;; Normally SERVER exits automatically once it has received a request.
    (thunk)))

(define-syntax-rule (with-http-server code body ...)
  (call-with-http-server code (lambda () body ...)))


(test-begin "lint")

(define (call-with-warnings thunk)
  (let ((port (open-output-string)))
    (parameterize ((guix-warning-port port))
      (thunk))
    (get-output-string port)))

(define-syntax-rule (with-warnings body ...)
  (call-with-warnings (lambda () body ...)))

(test-assert "description: not empty"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (description ""))))
                        (check-description-style pkg)))
                    "description should not be empty")))

(test-assert "description: does not start with an upper-case letter"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (description "bad description."))))
                        (check-description-style pkg)))
                    "description should start with an upper-case letter")))

(test-assert "description: may start with a digit"
  (string-null?
   (with-warnings
     (let ((pkg (dummy-package "x"
                  (description "2-component library."))))
       (check-description-style pkg)))))

(test-assert "description: may start with lower-case package name"
  (string-null?
   (with-warnings
     (let ((pkg (dummy-package "x"
                  (description "x is a dummy package."))))
       (check-description-style pkg)))))

(test-assert "description: two spaces after end of sentence"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (description "Bad. Quite bad."))))
                        (check-description-style pkg)))
                    "sentences in description should be followed by two spaces")))

(test-assert "description: end-of-sentence detection with abbreviations"
  (string-null?
   (with-warnings
     (let ((pkg (dummy-package "x"
                  (description
                   "E.g. Foo, i.e. Bar resp. Baz (a.k.a. DVD)."))))
       (check-description-style pkg)))))

(test-assert "synopsis: not empty"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (synopsis ""))))
                        (check-synopsis-style pkg)))
                    "synopsis should not be empty")))

(test-assert "synopsis: does not start with an upper-case letter"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (synopsis "bad synopsis."))))
                        (check-synopsis-style pkg)))
                    "synopsis should start with an upper-case letter")))

(test-assert "synopsis: may start with a digit"
  (string-null?
   (with-warnings
     (let ((pkg (dummy-package "x"
                  (synopsis "5-dimensional frobnicator"))))
       (check-synopsis-style pkg)))))

(test-assert "synopsis: ends with a period"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (synopsis "Bad synopsis."))))
                        (check-synopsis-style pkg)))
                    "no period allowed at the end of the synopsis")))

(test-assert "synopsis: ends with 'etc.'"
  (string-null? (with-warnings
                  (let ((pkg (dummy-package "x"
                               (synopsis "Foo, bar, etc."))))
                    (check-synopsis-style pkg)))))

(test-assert "synopsis: starts with 'A'"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (synopsis "A bad synopŝis"))))
                        (check-synopsis-style pkg)))
                    "no article allowed at the beginning of the synopsis")))

(test-assert "synopsis: starts with 'An'"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (synopsis "An awful synopsis"))))
                        (check-synopsis-style pkg)))
                    "no article allowed at the beginning of the synopsis")))

(test-assert "synopsis: starts with 'a'"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (synopsis "a bad synopsis"))))
                        (check-synopsis-style pkg)))
                    "no article allowed at the beginning of the synopsis")))

(test-assert "synopsis: starts with 'an'"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (synopsis "an awful synopsis"))))
                        (check-synopsis-style pkg)))
                    "no article allowed at the beginning of the synopsis")))

(test-assert "synopsis: too long"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (synopsis (make-string 80 #\x)))))
                        (check-synopsis-style pkg)))
                    "synopsis should be less than 80 characters long")))

(test-assert "synopsis: start with package name"
  (->bool
   (string-contains (with-warnings
                      (let ((pkg (dummy-package "x"
                                   (name "foo")
                                   (synopsis "foo, a nice package"))))
                        (check-synopsis-style pkg)))
                    "synopsis should not start with the package name")))

(test-assert "synopsis: start with package name prefix"
  (string-null?
   (with-warnings
     (let ((pkg (dummy-package "arb"
                  (synopsis "Arbitrary precision"))))
       (check-synopsis-style pkg)))))

(test-assert "synopsis: start with abbreviation"
  (string-null?
   (with-warnings
     (let ((pkg (dummy-package "uucp"
                  ;; Same problem with "APL interpreter", etc.
                  (synopsis "UUCP implementation")
                  (description "Imagine this is Taylor UUCP."))))
       (check-synopsis-style pkg)))))

(test-assert "inputs: pkg-config is probably a native input"
  (->bool
   (string-contains
     (with-warnings
       (let ((pkg (dummy-package "x"
                    (inputs `(("pkg-config" ,pkg-config))))))
         (check-inputs-should-be-native pkg)))
         "pkg-config should probably be a native input")))

(test-assert "patches: file names"
  (->bool
   (string-contains
     (with-warnings
       (let ((pkg (dummy-package "x"
                    (source
                     (origin
                       (method url-fetch)
                       (uri "someurl")
                       (sha256 "somesha")
                       (patches (list "/path/to/y.patch")))))))
         (check-patches pkg)))
     "file names of patches should start with the package name")))

(test-assert "home-page: wrong home-page"
  (->bool
   (string-contains
    (with-warnings
      (let ((pkg (package
                   (inherit (dummy-package "x"))
                   (home-page #f))))
        (check-home-page pkg)))
    "invalid")))

(test-assert "home-page: invalid URI"
  (->bool
   (string-contains
    (with-warnings
      (let ((pkg (package
                   (inherit (dummy-package "x"))
                   (home-page "foobar"))))
        (check-home-page pkg)))
    "invalid home page URL")))

(test-assert "home-page: host not found"
  (->bool
   (string-contains
    (with-warnings
      (let ((pkg (package
                   (inherit (dummy-package "x"))
                   (home-page "http://does-not-exist"))))
        (check-home-page pkg)))
    "domain not found")))

(test-skip (if %http-server-socket 0 1))
(test-assert "home-page: Connection refused"
  (->bool
   (string-contains
    (with-warnings
      (let ((pkg (package
                   (inherit (dummy-package "x"))
                   (home-page %local-url))))
        (check-home-page pkg)))
    "Connection refused")))

(test-skip (if %http-server-socket 0 1))
(test-equal "home-page: 200"
  ""
  (with-warnings
   (with-http-server 200
     (let ((pkg (package
                  (inherit (dummy-package "x"))
                  (home-page %local-url))))
       (check-home-page pkg)))))

(test-skip (if %http-server-socket 0 1))
(test-assert "home-page: 404"
  (->bool
   (string-contains
    (with-warnings
      (with-http-server 404
        (let ((pkg (package
                     (inherit (dummy-package "x"))
                     (home-page %local-url))))
          (check-home-page pkg))))
    "not reachable: 404")))

(test-end "lint")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;; Local Variables:
;; eval: (put 'with-http-server 'scheme-indent-function 1)
;; eval: (put 'with-warnings 'scheme-indent-function 0)
;; End:
