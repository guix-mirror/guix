;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gcrypt hash)
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
            (>>= (compare-contents (list out) (%test-substitute-urls))
                 (match-lambda
                   ((report)
                    (return
                     (and (string=? out (comparison-report-item report))
                          (bytevector=?
                           (comparison-report-local-sha256 report)
                           hash)
                          (comparison-report-match? report))))))))))))

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
            (>>= (compare-contents (list out) (%test-substitute-urls))
                 (match-lambda
                   ((report)
                    (return
                     (and (string=? out (comparison-report-item (pk report)))
                          (eq? 'mismatch (comparison-report-result report))
                          (bytevector=? hash
                                        (comparison-report-local-sha256
                                         report))
                          (match (comparison-report-narinfos report)
                            ((bad)
                             (bytevector=? wrong-hash
                                           (narinfo-hash->sha256
                                            (narinfo-hash bad))))))))))))))))

(test-assertm "inconclusive: no substitutes"
  (mlet* %store-monad ((drv  (gexp->derivation "foo" #~(mkdir #$output)))
                       (out -> (derivation->output-path drv))
                       (_    (built-derivations (list drv)))
                       (hash (query-path-hash* out)))
    (>>= (compare-contents (list out) (%test-substitute-urls))
         (match-lambda
           ((report)
            (return
             (and (string=? out (comparison-report-item report))
                  (comparison-report-inconclusive? report)
                  (null? (comparison-report-narinfos report))
                  (bytevector=? (comparison-report-local-sha256 report)
                                hash))))))))

(test-assertm "inconclusive: no local build"
  (let ((text (random-text)))
    (mlet* %store-monad ((drv (gexp->derivation "something"
                                                #~(list #$output #$text)))
                         (out -> (derivation->output-path drv))
                         (hash -> (sha256 #vu8())))
      (with-derivation-narinfo* drv (sha256 => hash)
        (>>= (compare-contents (list out) (%test-substitute-urls))
             (match-lambda
               ((report)
                (return
                 (and (string=? out (comparison-report-item report))
                      (comparison-report-inconclusive? report)
                      (not (comparison-report-local-sha256 report))
                      (match (comparison-report-narinfos report)
                        ((narinfo)
                         (bytevector=? (narinfo-hash->sha256
                                        (narinfo-hash narinfo))
                                       hash))))))))))))


(test-end)

;;; Local Variables:
;;; eval: (put 'with-derivation-narinfo* 'scheme-indent-function 2)
;;; End:
