;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
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
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

;; Test the (guix profiles) module.

(define %store
  (open-connection))

(define guile-for-build
  (package-derivation %store %bootstrap-guile))

;; Make it the default.
(%guile-for-build guile-for-build)


;; Example manifest entries.

(define guile-1.8.8
  (manifest-entry
    (name "guile")
    (version "1.8.8")
    (item "/gnu/store/...")
    (output "out")))

(define guile-2.0.9
  (manifest-entry
    (name "guile")
    (version "2.0.9")
    (item "/gnu/store/...")
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

(test-assert "manifest-add"
  (let* ((m0 (manifest '()))
         (m1 (manifest-add m0 (list guile-1.8.8)))
         (m2 (manifest-add m1 (list guile-2.0.9)))
         (m3 (manifest-add m2 (list guile-2.0.9:debug)))
         (m4 (manifest-add m3 (list guile-2.0.9:debug))))
    (and (match (manifest-entries m1)
           ((($ <manifest-entry> "guile" "1.8.8" "out")) #t)
           (_ #f))
         (match (manifest-entries m2)
           ((($ <manifest-entry> "guile" "2.0.9" "out")) #t)
           (_ #f))
         (equal? m3 m4))))

(test-assert "manifest-perform-transaction"
  (let* ((m0 (manifest (list guile-2.0.9 guile-2.0.9:debug)))
         (t1 (manifest-transaction
              (install (list guile-1.8.8))
              (remove (list (manifest-pattern (name "guile")
                                              (output "debug"))))))
         (t2 (manifest-transaction
              (remove (list (manifest-pattern (name "guile")
                                              (version "2.0.9")
                                              (output #f))))))
         (m1 (manifest-perform-transaction m0 t1))
         (m2 (manifest-perform-transaction m1 t2))
         (m3 (manifest-perform-transaction m0 t2)))
    (and (match (manifest-entries m1)
           ((($ <manifest-entry> "guile" "1.8.8" "out")) #t)
           (_ #f))
         (equal? m1 m2)
         (null? (manifest-entries m3)))))

(test-assert "profile-derivation"
  (run-with-store %store
    (mlet* %store-monad
        ((entry ->   (package->manifest-entry %bootstrap-guile))
         (guile      (package->derivation %bootstrap-guile))
         (drv        (profile-derivation (manifest (list entry))))
         (profile -> (derivation->output-path drv))
         (bindir ->  (string-append profile "/bin"))
         (_          (built-derivations (list drv))))
      (return (and (file-exists? (string-append bindir "/guile"))
                   (string=? (dirname (readlink bindir))
                             (derivation->output-path guile)))))))

(test-end "profiles")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'dummy-package 'scheme-indent-function 1)
;;; End:
