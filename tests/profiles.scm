;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-profiles)
  #:use-module (guix profiles)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

;; Test the (guix profile) module.


;; Example manifest entries.

(define guile-2.0.9
  (manifest-entry
    (name "guile")
    (version "2.0.9")
    (path "/gnu/store/...")
    (output "out")))

(define guile-2.0.9:debug
  (manifest-entry (inherit guile-2.0.9)
    (output "debug")))


(test-begin "profiles")

(test-assert "manifest-installed?"
  (let ((m (manifest (list guile-2.0.9 guile-2.0.9:debug))))
    (and (manifest-installed? m (manifest-pattern (name "guile")))
         (manifest-installed? m (manifest-pattern
                                  (name "guile") (output "debug")))
         (manifest-installed? m (manifest-pattern
                                  (name "guile") (output "out")
                                  (version "2.0.9")))
         (not (manifest-installed?
               m (manifest-pattern (name "guile") (version "1.8.8"))))
         (not (manifest-installed?
               m (manifest-pattern (name "guile") (output "foobar")))))))

(test-assert "manifest-matching-entries"
  (let* ((e (list guile-2.0.9 guile-2.0.9:debug))
         (m (manifest e)))
    (and (null? (manifest-matching-entries m
                                           (list (manifest-pattern
                                                   (name "python")))))
         (equal? e
                 (manifest-matching-entries m
                                            (list (manifest-pattern
                                                    (name "guile")
                                                    (output #f)))))
         (equal? (list guile-2.0.9)
                 (manifest-matching-entries m
                                            (list (manifest-pattern
                                                    (name "guile")
                                                    (version "2.0.9"))))))))

(test-assert "manifest-remove"
  (let* ((m0 (manifest (list guile-2.0.9 guile-2.0.9:debug)))
         (m1 (manifest-remove m0
                              (list (manifest-pattern (name "guile")))))
         (m2 (manifest-remove m1
                              (list (manifest-pattern (name "guile"))))) ; same
         (m3 (manifest-remove m2
                              (list (manifest-pattern
                                      (name "guile") (output "debug")))))
         (m4 (manifest-remove m3
                              (list (manifest-pattern (name "guile"))))))
    (match (manifest-entries m2)
      ((($ <manifest-entry> "guile" "2.0.9" "debug"))
       (and (equal? m1 m2)
            (null? (manifest-entries m3))
            (null? (manifest-entries m4)))))))

(test-end "profiles")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'dummy-package 'scheme-indent-function 1)
;;; End:
