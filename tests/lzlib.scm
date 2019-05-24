;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (test-lzlib)
  #:use-module (guix lzlib)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

;; Test the (guix lzlib) module.

(define-syntax-rule (test-assert* description exp)
  (begin
    (unless (lzlib-available?)
      (test-skip 1))
    (test-assert description exp)))

(test-begin "lzlib")

(define (compress-and-decompress data)
  "DATA must be a bytevector."
  (pk "Uncompressed bytes:" (bytevector-length data))
  (match (pipe)
    ((parent . child)
     (match (primitive-fork)
       (0                               ;compress
        (dynamic-wind
          (const #t)
          (lambda ()
            (close-port parent)
            (call-with-lzip-output-port child
              (lambda (port)
                (put-bytevector port data))))
          (lambda ()
            (primitive-exit 0))))
       (pid                             ;decompress
        (begin
          (close-port child)
          (let ((received (call-with-lzip-input-port parent
                            (lambda (port)
                              (get-bytevector-all port)))))
            (match (waitpid pid)
              ((_ . status)
               (pk "Status" status)
               (pk "Length data" (bytevector-length data) "received" (bytevector-length received))
               ;; The following loop is a debug helper.
               (let loop ((i 0))
                 (if (and (< i (bytevector-length received))
                          (= (bytevector-u8-ref received i)
                             (bytevector-u8-ref data i)))
                     (loop (+ 1 i))
                     (pk "First diff at index" i)))
               (and (zero? status)
                    (port-closed? parent)
                    (bytevector=? received data)))))))))))

(test-assert* "null bytevector"
  (compress-and-decompress (make-bytevector (+ (random 100000)
                                               (* 20 1024)))))

(test-assert* "random bytevector"
  (compress-and-decompress (random-bytevector (+ (random 100000)
                                                 (* 20 1024)))))
(test-assert* "small bytevector"
  (compress-and-decompress (random-bytevector 127)))

(test-assert* "1 bytevector"
  (compress-and-decompress (random-bytevector 1)))

(test-assert* "Bytevector of size relative to Lzip internal buffers (2 * dictionary)"
  (compress-and-decompress
   (random-bytevector
    (* 2 (car (car (assoc-ref (@@ (guix lzlib) %compression-levels)
                              (@@ (guix lzlib) %default-compression-level))))))))

(test-assert* "Bytevector of size relative to Lzip internal buffers (64KiB)"
  (compress-and-decompress (random-bytevector (* 64 1024))))

(test-assert* "Bytevector of size relative to Lzip internal buffers (64KiB-1)"
  (compress-and-decompress (random-bytevector (1- (* 64 1024)))))

(test-assert* "Bytevector of size relative to Lzip internal buffers (64KiB+1)"
  (compress-and-decompress (random-bytevector (1+ (* 64 1024)))))

(test-assert* "Bytevector of size relative to Lzip internal buffers (1MiB)"
  (compress-and-decompress (random-bytevector (* 1024 1024))))

(test-assert* "Bytevector of size relative to Lzip internal buffers (1MiB-1)"
  (compress-and-decompress (random-bytevector (1- (* 1024 1024)))))

(test-assert* "Bytevector of size relative to Lzip internal buffers (1MiB+1)"
  (compress-and-decompress (random-bytevector (1+ (* 1024 1024)))))

(test-end)
