;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-status)
  #:use-module (guix status)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

(test-begin "status")

(test-equal "compute-status, no-op"
  (build-status)
  (let-values (((port get-status)
                (build-event-output-port compute-status)))
    (display "foo\nbar\n\baz\n" port)
    (get-status)))

(test-equal "compute-status, builds + substitutes"
  (list (build-status
         (building '("foo.drv"))
         (downloading (list (download "bar" "http://example.org/bar"
                                      #:size 500
                                      #:start 'now))))
        (build-status
         (building '("foo.drv"))
         (downloading (list (download "bar" "http://example.org/bar"
                                      #:size 500
                                      #:transferred 42
                                      #:start 'now))))
        (build-status
         (builds-completed '("foo.drv"))
         (downloads-completed (list (download "bar" "http://example.org/bar"
                                              #:size 500
                                              #:transferred 500
                                              #:start 'now
                                              #:end 'now)))))
  (let-values (((port get-status)
                (build-event-output-port (lambda (event status)
                                           (compute-status event status
                                                           #:current-time
                                                           (const 'now))))))
    (display "@ build-started foo.drv\n" port)
    (display "@ substituter-started bar\n" port)
    (display "@ download-started bar http://example.org/bar 500\n" port)
    (display "various\nthings\nget\nwritten\n" port)
    (let ((first (get-status)))
      (display "@ download-progress bar http://example.org/bar 500 42\n"
               port)
      (let ((second (get-status)))
        (display "@ download-progress bar http://example.org/bar 500 84\n"
                 port)
        (display "@ build-succeeded foo.drv\n" port)
        (display "@ download-succeeded bar http://example.org/bar 500\n" port)
        (display "Almost done!\n" port)
        (display "@ substituter-succeeded bar\n" port)
        (list first second (get-status))))))

(test-equal "compute-status, missing events"
  (list (build-status
         (building '("foo.drv"))
         (downloading (list (download "baz" "http://example.org/baz"
                                      #:size 500
                                      #:transferred 42
                                      #:start 'now)
                            (download "bar" "http://example.org/bar"
                                      #:size 999
                                      #:transferred 0
                                      #:start 'now))))
        (build-status
         (builds-completed '("foo.drv"))
         (downloads-completed (list (download "baz" "http://example.org/baz"
                                              #:size 500
                                              #:transferred 500
                                              #:start 'now
                                              #:end 'now)
                                    (download "bar" "http://example.org/bar"
                                              #:size 999
                                              #:transferred 999
                                              #:start 'now
                                              #:end 'now)))))
  ;; Below we omit 'substituter-started' events and the like.
  (let-values (((port get-status)
                (build-event-output-port (lambda (event status)
                                           (compute-status event status
                                                           #:current-time
                                                           (const 'now))))))
    (display "@ build-started foo.drv\n" port)
    (display "@ download-started bar http://example.org/bar 999\n" port)
    (display "various\nthings\nget\nwritten\n" port)
    (display "@ download-progress baz http://example.org/baz 500 42\n"
             port)
    (let ((first (get-status)))
      (display "@ build-succeeded foo.drv\n" port)
      (display "@ download-succeeded bar http://example.org/bar 999\n" port)
      (display "Almost done!\n" port)
      (display "@ substituter-succeeded baz\n" port)
      (list first (get-status)))))

(test-equal "build-output-port, UTF-8"
  '((build-log #f "lambda is λ!\n"))
  (let-values (((port get-status) (build-event-output-port cons '()))
               ((bv)              (string->utf8 "lambda is λ!\n")))
    (put-bytevector port bv)
    (force-output port)
    (get-status)))

(test-equal "current-build-output-port, UTF-8 + garbage"
  ;; What about a mixture of UTF-8 + garbage?
  (let ((replacement (cond-expand
                      ((and guile-2 (not guile-2.2)) "?")
                      (else "�"))))
    `((build-log #f ,(string-append "garbage: " replacement "lambda: λ\n"))))
  (let-values (((port get-status) (build-event-output-port cons '())))
    (display "garbage: " port)
    (put-bytevector port #vu8(128))
    (put-bytevector port (string->utf8 "lambda: λ\n"))
    (force-output port)
    (get-status)))

(test-equal "compute-status, multiplexed build output"
  (list (build-status
         (building '("foo.drv"))
         (downloading (list (download "bar" "http://example.org/bar"
                                      #:size 999
                                      #:start 'now))))
        (build-status
         (building '("foo.drv"))
         (downloading (list (download "bar" "http://example.org/bar"
                                      #:size 999
                                      #:transferred 42
                                      #:start 'now))))
        (build-status
         ;; XXX: Should "bar.drv" be present twice?
         (builds-completed '("bar.drv" "foo.drv"))
         (downloads-completed (list (download "bar" "http://example.org/bar"
                                              #:size 999
                                              #:transferred 999
                                              #:start 'now
                                              #:end 'now)))))
  (let-values (((port get-status)
                (build-event-output-port (lambda (event status)
                                           (compute-status event status
                                                           #:current-time
                                                           (const 'now)
                                                           #:derivation-path->output-path
                                                           (match-lambda
                                                             ("bar.drv" "bar")))))))
    (display "@ build-started foo.drv 121\n" port)
    (display "@ build-started bar.drv 144\n" port)
    (display "@ build-log 121 6\nHello!" port)
    (display "@ build-log 144 50
@ download-started bar http://example.org/bar 999\n" port)
    (let ((first (get-status)))
      (display "@ build-log 121 30\n@ build-started FAKE!.drv 555\n")
      (display "@ build-log 144 54
@ download-progress bar http://example.org/bar 999 42\n"
               port)
      (let ((second (get-status)))
        (display "@ download-succeeded bar http://example.org/bar 999\n" port)
        (display "@ build-succeeded foo.drv\n" port)
        (display "@ build-succeeded bar.drv\n" port)
        (list first second (get-status))))))

(test-end "status")
