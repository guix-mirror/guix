;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix tests http)
  #:use-module ((gcrypt hash) #:prefix gcrypt:)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix serialization)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix base32)
  #:use-module (guix narinfo)
  #:use-module (guix scripts challenge)
  #:use-module ((guix build utils) #:select (find-files))
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

(define query-path-hash*
  (store-lift query-path-hash))

(define (query-path-size item)
  (mlet %store-monad ((info (query-path-info* item)))
    (return (path-info-nar-size info))))

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
                         (hash -> (gcrypt:sha256 #vu8())))
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
(define (make-narinfo item size hash)
  (format #f "StorePath: ~a
Compression: none
URL: nar/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo
NarSize: ~d
NarHash: sha256:~a
References: ~%" item size (bytevector->nix-base32-string hash)))

(define (call-mismatch-test proc)
  "Pass PROC a <comparison-report> for a mismatch and return its return
value."

  ;; Pretend we have two different results for the same store item, ITEM, with
  ;; "/bin/guile" differing between the two nars.
  (mlet* %store-monad
      ((drv1 (package->derivation %bootstrap-guile))
       (drv2 (gexp->derivation
              "broken-guile"
              (with-imported-modules '((guix build utils))
                #~(begin
                    (use-modules (guix build utils))
                    (copy-recursively #$drv1 #$output)
                    (chmod (string-append #$output "/bin/guile")
                           #o755)
                    (call-with-output-file (string-append
                                            #$output
                                            "/bin/guile")
                      (lambda (port)
                        (display "corrupt!" port)))))))
       (out1 -> (derivation->output-path drv1))
       (out2 -> (derivation->output-path drv2))
       (item -> (string-append (%store-prefix) "/"
                               (bytevector->nix-base32-string
                                (random-bytevector 32))
                               "-foo"
                               (number->string (current-time) 16))))
    (mbegin %store-monad
      (built-derivations (list drv1 drv2))
      (mlet* %store-monad ((size1 (query-path-size out1))
                           (size2 (query-path-size out2))
                           (hash1 (query-path-hash* out1))
                           (hash2 (query-path-hash* out2))
                           (nar1 -> (call-with-bytevector-output-port
                                     (lambda (port)
                                       (write-file out1 port))))
                           (nar2 -> (call-with-bytevector-output-port
                                     (lambda (port)
                                       (write-file out2 port)))))
        (parameterize ((%http-server-port 9000))
          (with-http-server `((200 ,(make-narinfo item size1 hash1))
                              (200 ,nar1))
            (parameterize ((%http-server-port 9001))
              (with-http-server `((200 ,(make-narinfo item size2 hash2))
                                  (200 ,nar2))
                (mlet* %store-monad ((urls -> (list (%local-url 9000)
                                                    (%local-url 9001)))
                                     (reports (compare-contents (list item)
                                                                urls)))
                  (pk 'report reports)
                  (return (proc (car reports))))))))))))

(test-assertm "differing-files"
  (call-mismatch-test
   (lambda (report)
     (equal? (differing-files report) '("/bin/guile")))))

(test-assertm "call-with-mismatches"
  (call-mismatch-test
   (lambda (report)
     (call-with-mismatches
      report
      (lambda (directory1 directory2)
        (let* ((files1 (find-files directory1))
               (files2 (find-files directory2))
               (files  (map (cute string-drop <> (string-length directory1))
                            files1)))
          (and (equal? files
                       (map (cute string-drop <> (string-length directory2))
                            files2))
               (equal? (remove (lambda (file)
                                 (file=? (string-append directory1 "/" file)
                                         (string-append directory2 "/" file)))
                               files)
                       '("/bin/guile")))))))))

(test-end)

;;; Local Variables:
;;; eval: (put 'with-derivation-narinfo* 'scheme-indent-function 2)
;;; End:
