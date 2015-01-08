;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-34)
  #:use-module (rnrs bytevectors)
  #:use-module (web uri)
  #:export (open-connection-for-tests
            random-text
            random-bytevector
            mock
            with-derivation-narinfo
            dummy-package))

;;; Commentary:
;;;
;;; This module provide shared infrastructure for the test suite.  For
;;; internal use only.
;;;
;;; Code:

(define (open-connection-for-tests)
  "Open a connection to the build daemon for tests purposes and return it."
  (guard (c ((nix-error? c)
             (format (current-error-port)
                     "warning: build daemon error: ~s~%" c)
             #f))
    (let ((store (open-connection)))
      ;; Make sure we build everything by ourselves.
      (set-build-options store #:use-substitutes? #f)

      ;; Use the bootstrap Guile when running tests, so we don't end up
      ;; building everything in the temporary test store.
      (%guile-for-build (package-derivation store %bootstrap-guile))

      store)))

(define %seed
  (seed->random-state (logxor (getpid) (car (gettimeofday)))))

(define (random-text)
  "Return the hexadecimal representation of a random number."
  (number->string (random (expt 2 256) %seed) 16))

(define (random-bytevector n)
  "Return a random bytevector of N bytes."
  (let ((bv (make-bytevector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (bytevector-u8-set! bv i (random 256 %seed))
            (loop (1+ i)))
          bv))))

(define-syntax-rule (mock (module proc replacement) body ...)
  "Within BODY, replace the definition of PROC from MODULE with the definition
given by REPLACEMENT."
  (let* ((m (resolve-module 'module))
         (original (module-ref m 'proc)))
    (dynamic-wind
      (lambda () (module-set! m 'proc replacement))
      (lambda () body ...)
      (lambda () (module-set! m 'proc original)))))


;;;
;;; Narinfo files, as used by the substituter.
;;;

(define* (derivation-narinfo drv #:optional (nar "example.nar"))
  "Return the contents of the narinfo corresponding to DRV; NAR should be the
file name of the archive containing the substitute for DRV."
  (format #f "StorePath: ~a
URL: ~a
Compression: none
NarSize: 1234
References: 
System: ~a
Deriver: ~a~%"
          (derivation->output-path drv)       ; StorePath
          nar                                 ; URL
          (derivation-system drv)             ; System
          (basename
           (derivation-file-name drv))))      ; Deriver

(define (call-with-derivation-narinfo drv thunk)
  "Call THUNK in a context where fake substituter data, as read by 'guix
substitute-binary', has been installed for DRV."
  (let* ((output  (derivation->output-path drv))
         (dir     (uri-path
                   (string->uri (getenv "GUIX_BINARY_SUBSTITUTE_URL"))))
         (info    (string-append dir "/nix-cache-info"))
         (narinfo (string-append dir "/" (store-path-hash-part output)
                                 ".narinfo")))
    (dynamic-wind
      (lambda ()
        (call-with-output-file info
          (lambda (p)
            (format p "StoreDir: ~a\nWantMassQuery: 0\n"
                    (%store-prefix))))
        (call-with-output-file narinfo
          (lambda (p)
            (display (derivation-narinfo drv) p))))
      thunk
      (lambda ()
        (delete-file narinfo)
        (delete-file info)))))

(define-syntax-rule (with-derivation-narinfo drv body ...)
  "Evaluate BODY in a context where DRV looks substitutable from the
substituter's viewpoint."
  (call-with-derivation-narinfo drv
    (lambda ()
      body ...)))

(define-syntax-rule (dummy-package name* extra-fields ...)
  "Return a \"dummy\" package called NAME*, with all its compulsory fields
initialized with default values, and with EXTRA-FIELDS set as specified."
  (package extra-fields ...
           (name name*) (version "0") (source #f)
           (build-system gnu-build-system)
           (synopsis #f) (description #f)
           (home-page #f) (license #f)))

;; Local Variables:
;; eval: (put 'call-with-derivation-narinfo 'scheme-indent-function 1)
;; End:

;;; tests.scm ends here
