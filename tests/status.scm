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
  #:use-module (srfi srfi-64))

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

(test-end "status")
