;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-channels)
  #:use-module (guix channels)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "channels")

(define* (make-instance #:key
                        (name 'fake)
                        (commit "cafebabe")
                        (spec #f))
  (define instance-dir (mkdtemp! "/tmp/checkout.XXXXXX"))
  (and spec
       (with-output-to-file (string-append instance-dir "/.guix-channel")
         (lambda _ (format #t "~a" spec))))
  ((@@ (guix channels) channel-instance)
   name commit instance-dir))

(define instance--boring (make-instance))
(define instance--no-deps
  (make-instance #:spec
                 '(channel
                   (version 0)
                   (dependencies
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel"))))))
(define instance--simple
  (make-instance #:spec
                 '(channel
                   (version 0)
                   (dependencies
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel"))))))
(define instance--with-dupes
  (make-instance #:spec
                 '(channel
                   (version 0)
                   (dependencies
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel"))
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel")
                     (commit "abc1234"))
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel-elsewhere"))))))

(define read-channel-metadata
  (@@ (guix channels) read-channel-metadata))


(test-equal "read-channel-metadata returns #f if .guix-channel does not exist"
  #f
  (read-channel-metadata instance--boring))

(test-assert "read-channel-metadata returns <channel-metadata>"
  (every (@@ (guix channels) channel-metadata?)
         (map read-channel-metadata
              (list instance--no-deps
                    instance--simple
                    instance--with-dupes))))

(test-assert "read-channel-metadata dependencies are channels"
  (let ((deps ((@@ (guix channels) channel-metadata-dependencies)
               (read-channel-metadata instance--simple))))
    (match deps
      (((? channel? dep)) #t)
      (_ #f))))

(test-assert "latest-channel-instances includes channel dependencies"
  (let* ((channel (channel
                   (name 'test)
                   (url "test")))
         (test-dir (channel-instance-checkout instance--simple)))
    (mock ((guix git) latest-repository-commit
           (lambda* (store url #:key ref)
             (match url
               ("test" (values test-dir 'whatever))
               (_ (values "/not-important" 'not-important)))))
          (let ((instances (latest-channel-instances #f (list channel))))
            (and (eq? 2 (length instances))
                 (lset= eq?
                        '(test test-channel)
                        (map (compose channel-name channel-instance-channel)
                             instances)))))))

(test-assert "latest-channel-instances excludes duplicate channel dependencies"
  (let* ((channel (channel
                   (name 'test)
                   (url "test")))
         (test-dir (channel-instance-checkout instance--with-dupes)))
    (mock ((guix git) latest-repository-commit
           (lambda* (store url #:key ref)
             (match url
               ("test" (values test-dir 'whatever))
               (_ (values "/not-important" 'not-important)))))
          (let ((instances (latest-channel-instances #f (list channel))))
            (and (eq? 2 (length instances))
                 (lset= eq?
                        '(test test-channel)
                        (map (compose channel-name channel-instance-channel)
                             instances))
                 ;; only the most specific channel dependency should remain,
                 ;; i.e. the one with a specified commit.
                 (find (lambda (instance)
                         (and (eq? (channel-name
                                    (channel-instance-channel instance))
                                   'test-channel)
                              (eq? (channel-commit
                                    (channel-instance-channel instance))
                                   'abc1234)))
                       instances))))))

(test-end "channels")
