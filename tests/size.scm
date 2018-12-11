;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-size)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix tests)
  #:use-module (guix scripts size)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))


(test-begin "size")

(test-assertm "store-profile"
  (mlet* %store-monad ((file1 (gexp->derivation "file1"
                                                #~(symlink #$%bootstrap-guile
                                                           #$output)))
                       (file2 (text-file* "file2"
                                          "the file => " file1)))
    (define (matching-profile item)
      (lambda (profile)
        (string=? item (profile-file profile))))

    (mbegin %store-monad
      (built-derivations (list file2))
      (mlet %store-monad ((profiles (store-profile
                                     (list (derivation->output-path file2))))
                          (bash     (interned-file
                                     (search-bootstrap-binary
                                      "bash" (%current-system)) "bash"
                                      #:recursive? #t))
                          (guile    (package->derivation %bootstrap-guile)))
        (define (lookup-profile item)
          (find (matching-profile (if (derivation? item)
                                      (derivation->output-path item)
                                      item))
                profiles))

        (letrec-syntax ((match* (syntax-rules (=>)
                                  ((_ ((drv => profile) rest ...) body)
                                   (match (lookup-profile drv)
                                     ((? profile? profile)
                                      (match* (rest ...) body))))
                                  ((_ () body)
                                   body))))
          ;; Make sure we get all three profiles with sensible values.
          (return (and (= (length profiles) 4)
                       (match* ((file1 => profile1)
                                (file2 => profile2)
                                (guile => profile3)
                                (bash  => profile4)) ;dependency of GUILE
                         (and (> (profile-closure-size profile2) 0)
                              (= (profile-closure-size profile2)
                                 (+ (profile-self-size profile1)
                                    (profile-self-size profile2)
                                    (profile-self-size profile3)
                                    (profile-self-size profile4))))))))))))

(test-assertm "store-profile with multiple items"
  (mlet* %store-monad ((file1 (gexp->derivation "file1"
                                                #~(symlink #$%bootstrap-guile
                                                           #$output)))
                       (file2 (text-file* "file2"
                                          "the file => " file1)))
    (mbegin %store-monad
      (built-derivations (list file2))
      (mlet %store-monad ((profiles  (store-profile
                                      (list (derivation->output-path file2)
                                            (derivation->output-path file1))))
                          (reference (store-profile
                                      (list (derivation->output-path file2)))))
        (return (and (= (length profiles) 4)
                     (lset= equal? profiles reference)))))))

(test-end "size")

;;; Local Variables:
;;; eval: (put 'match* 'scheme-indent-function 1)
;;; End:
