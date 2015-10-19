;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-challenge)
  #:use-module (guix tests)
  #:use-module (guix hash)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix scripts challenge)
  #:use-module (guix scripts substitute)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match))

(define %store
  (open-connection-for-tests))

(define query-path-hash*
  (store-lift query-path-hash))

(define-syntax-rule (test-assertm name exp)
  (test-assert name
    (run-with-store %store exp
                    #:guile-for-build (%guile-for-build))))

(define* (call-with-derivation-narinfo* drv thunk hash)
  (lambda (store)
    (with-derivation-narinfo drv (sha256 => hash)
      (values (run-with-store store (thunk)) store))))

(define-syntax with-derivation-narinfo*
  (syntax-rules (sha256 =>)
    ((_ drv (sha256 => hash) body ...)
     (call-with-derivation-narinfo* drv
       (lambda () body ...)
       hash))))


(test-begin "challenge")

(test-assertm "no discrepancies"
  (let ((text (random-text)))
    (mlet* %store-monad ((drv (gexp->derivation "something"
                                                #~(call-with-output-file
                                                      #$output
                                                    (lambda (port)
                                                      (display #$text port)))))
                         (out -> (derivation->output-path drv)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (mlet %store-monad ((hash (query-path-hash* out)))
          (with-derivation-narinfo* drv (sha256 => hash)
            (>>= (discrepancies (list out) (%test-substitute-urls))
                 (lift1 null? %store-monad))))))))

(test-assertm "one discrepancy"
  (let ((text (random-text)))
    (mlet* %store-monad ((drv (gexp->derivation "something"
                                                #~(call-with-output-file
                                                      #$output
                                                    (lambda (port)
                                                      (display #$text port)))))
                         (out -> (derivation->output-path drv)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (mlet* %store-monad ((hash (query-path-hash* out))
                             (wrong-hash
                              -> (let* ((w (bytevector-copy hash))
                                        (b (bytevector-u8-ref w 0)))
                                   (bytevector-u8-set! w 0
                                                       (modulo (+ b 1) 128))
                                   w)))
          (with-derivation-narinfo* drv (sha256 => wrong-hash)
            (>>= (discrepancies (list out) (%test-substitute-urls))
                 (match-lambda
                   ((discrepancy)
                    (return
                     (and (string=? out (discrepancy-item discrepancy))
                          (bytevector=? hash
                                        (discrepancy-local-sha256
                                         discrepancy))
                          (match (discrepancy-narinfos discrepancy)
                            ((bad)
                             (bytevector=? wrong-hash
                                           (narinfo-hash->sha256
                                            (narinfo-hash bad))))))))))))))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'with-derivation-narinfo* 'scheme-indent-function 2)
;;; End:
