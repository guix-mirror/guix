;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-cache)
  #:use-module (guix cache)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (ice-9 match))

(test-begin "cache")

(test-equal "remove-expired-cache-entries"
  '("o" "l" "d")
  (let* ((removed '())
         (now     (time-second (current-time time-monotonic)))
         (ttl     100)
         (stamp   (match-lambda
                    ((or "n" "e" "w") (+ now 100))
                    ((or "o" "l" "d") (- now 100))))
         (delete  (lambda (entry)
                    (set! removed (cons entry removed)))))
    (remove-expired-cache-entries (reverse '("n" "e" "w"
                                             "o" "l" "d"))
                                  #:entry-expiration stamp
                                  #:delete-entry delete)
    removed))

(define-syntax-rule (test-cache-cleanup cache exp ...)
  (call-with-temporary-directory
   (lambda (cache)
     (let* ((deleted '())
            (delete! (lambda (entry)
                       (set! deleted (cons entry deleted)))))
       exp ...
       (maybe-remove-expired-cache-entries cache
                                           (const '("a" "b" "c"))
                                           #:entry-expiration (const 0)
                                           #:delete-entry delete!)
       (reverse deleted)))))

(test-equal "maybe-remove-expired-cache-entries, first cleanup"
  '("a" "b" "c")
  (test-cache-cleanup cache))

(test-equal "maybe-remove-expired-cache-entries, no cleanup needed"
  '()
  (test-cache-cleanup cache
    (call-with-output-file (string-append cache "/last-expiry-cleanup")
      (lambda (port)
        (display (+ (time-second (current-time time-monotonic)) 100)
                 port)))))

(test-equal "maybe-remove-expired-cache-entries, cleanup needed"
  '("a" "b" "c")
  (test-cache-cleanup cache
    (call-with-output-file (string-append cache "/last-expiry-cleanup")
      (lambda (port)
        (display 0 port)))))

(test-end "cache")

;;; Local Variables:
;;; eval: (put 'test-cache-cleanup 'scheme-indent-function 1)
;;; End:
