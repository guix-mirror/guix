;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix store)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (nix-server?
            nix-server-major-version
            nix-server-minor-version
            nix-server-socket

            open-connection
            set-build-options
            add-text-to-store
            add-to-store
            build-derivations))

(define %protocol-version #x109)

(define %worker-magic-1 #x6e697863)
(define %worker-magic-2 #x6478696f)

(define (protocol-major magic)
  (logand magic #xff00))
(define (protocol-minor magic)
  (logand magic #x00ff))

(define-syntax define-enumerate-type
  (syntax-rules ()
    ((_ name->int (name id) ...)
     (define-syntax name->int
       (syntax-rules (name ...)
         ((_ name) id) ...)))))

(define-enumerate-type operation-id
  ;; operation numbers from worker-protocol.hh
  (quit 0)
  (valid-path? 1)
  (has-substitutes? 3)
  (query-path-hash 4)
  (query-references 5)
  (query-referrers 6)
  (add-to-store 7)
  (add-text-to-store 8)
  (build-derivations 9)
  (ensure-path 10)
  (add-temp-root 11)
  (add-indirect-root 12)
  (sync-with-gc 13)
  (find-roots 14)
  (export-path 16)
  (query-deriver 18)
  (set-options 19)
  (collect-garbage 20)
  (query-substitutable-path-info 21)
  (query-derivation-outputs 22)
  (query-valid-paths 23)
  (query-failed-paths 24)
  (clear-failed-paths 25)
  (query-path-info 26)
  (import-paths 27)
  (query-derivation-output-names 28))

(define-enumerate-type hash-algo
  ;; hash.hh
  (md5 1)
  (sha1 2)
  (sha256 3))

(define %nix-state-dir "/nix/var/nix")
(define %default-socket-path
  (string-append %nix-state-dir "/daemon-socket/socket"))


;; serialize.cc

(define (write-int n p)
  (let ((b (make-bytevector 8 0)))
    (bytevector-u32-set! b 0 n (endianness little))
    (put-bytevector p b)))

(define (read-int p)
  (let ((b (get-bytevector-n p 8)))
    (bytevector-u32-ref b 0 (endianness little))))

(define (write-long-long n p)
  (let ((b (make-bytevector 8 0)))
    (bytevector-u64-set! b 0 n (endianness little))
    (put-bytevector p b)))

(define write-padding
  (let ((zero (make-bytevector 8 0)))
    (lambda (n p)
      (let ((m (modulo n 8)))
        (or (zero? m)
            (put-bytevector p zero 0 (- 8 m)))))))

(define (write-string s p)
  (let ((b (string->utf8 s)))
    (write-int (bytevector-length b) p)
    (put-bytevector p b)
    (write-padding (bytevector-length b) p)))

(define (read-string p)
  (let* ((len (read-int p))
         (m   (modulo len 8))
         (bv  (get-bytevector-n p len))
         (str (utf8->string bv)))
    (or (zero? m)
        (get-bytevector-n p (- 8 m)))
    str))

(define (write-string-list l p)
  (write-int (length l) p)
  (for-each (cut write-string <> p) l))

(define (read-store-path p)
  (read-string p))                                ; TODO: assert path

(define (write-contents file p)
  "Write the contents of FILE to output port P."
  (define (dump in size)
    (define buf-size 65536)
    (define buf (make-bytevector buf-size))

    (let loop ((left size))
      (if (<= left 0)
          0
          (let ((read (get-bytevector-n! in buf 0 buf-size)))
            (if (eof-object? read)
                left
                (begin
                  (put-bytevector p buf 0 read)
                  (loop (- left read))))))))

  (let ((size (stat:size (lstat file))))
    (write-string "contents" p)
    (write-long-long size p)
    (call-with-input-file file
      (lambda (p)
        (dump p size)))
    (write-padding size p)))

(define (write-file f p)
  (define %archive-version-1 "nix-archive-1")

  (let ((s (lstat f)))
    (write-string %archive-version-1 p)
    (write-string "(" p)
    (case (stat:type s)
      ((regular)
       (write-string "type" p)
       (write-string "regular" p)
       (if (not (zero? (logand (stat:mode s) #o100)))
           (begin
             (write-string "executable" p)
             (write-string "" p)))
       (write-contents f p)
       (write-string ")" p))
      ((directory)
       (write-string "type" p)
       (write-string "directory" p)
       (error "ENOSYS"))
      (else
       (error "ENOSYS")))))

(define-syntax write-arg
  (syntax-rules (integer boolean file string string-list)
    ((_ integer arg p)
     (write-int arg p))
    ((_ boolean arg p)
     (write-int (if arg 1 0) p))
    ((_ file arg p)
     (write-file arg p))
    ((_ string arg p)
     (write-string arg p))
    ((_ string-list arg p)
     (write-string-list arg p))))

(define-syntax read-arg
  (syntax-rules (integer boolean string store-path)
    ((_ integer p)
     (read-int p))
    ((_ boolean p)
     (not (zero? (read-int p))))
    ((_ string p)
     (read-string p))
    ((_ store-path p)
     (read-store-path p))))


;; remote-store.cc

(define-record-type <nix-server>
  (%make-nix-server socket major minor)
  nix-server?
  (socket nix-server-socket)
  (major  nix-server-major-version)
  (minor  nix-server-minor-version))

(define* (open-connection #:optional (file %default-socket-path))
  (let ((s (with-fluids ((%default-port-encoding #f))
             ;; This trick allows use of the `scm_c_read' optimization.
             (socket PF_UNIX SOCK_STREAM 0)))
        (a (make-socket-address PF_UNIX file)))
    (connect s a)
    (write-int %worker-magic-1 s)
    (let ((r (read-int s)))
      (and (eqv? r %worker-magic-2)
           (let ((v (read-int s)))
             (and (eqv? (protocol-major %protocol-version)
                        (protocol-major v))
                  (begin
                    (write-int %protocol-version s)
                    (let ((s (%make-nix-server s
                                               (protocol-major v)
                                               (protocol-minor v))))
                      (process-stderr s)
                      s))))))))

(define (process-stderr server)
  (define p
    (nix-server-socket server))

  ;; magic cookies from worker-protocol.hh
  (define %stderr-next  #x6f6c6d67)
  (define %stderr-read  #x64617461)               ; data needed from source
  (define %stderr-write #x64617416)               ; data for sink
  (define %stderr-last  #x616c7473)
  (define %stderr-error #x63787470)

  (let ((k (read-int p)))
    (cond ((= k %stderr-write)
           (read-string p))
          ((= k %stderr-read)
           (let ((len (read-int p)))
             (read-string p)                      ; FIXME: what to do?
             ))
          ((= k %stderr-next)
           (let ((s (read-string p)))
             (display s (current-error-port))
             s))
          ((= k %stderr-error)
           (let ((error  (read-string p))
                 (status (if (>= (nix-server-minor-version server) 8)
                             (read-int p)
                             1)))
             (format (current-error-port) "error: ~a (status: ~a)~%"
                     error status)
             error))
          ((= k %stderr-last)
           #t)
          (else
           (error "invalid standard error code" k)))))

(define* (set-build-options server
                            #:key keep-failed? keep-going? try-fallback?
                            (verbosity 0)
                            (max-build-jobs (current-processor-count))
                            (max-silent-time 3600)
                            (use-build-hook? #t)
                            (build-verbosity 0)
                            (log-type 0)
                            (print-build-trace #t))
  ;; Must be called after `open-connection'.

  (define socket
    (nix-server-socket server))

  (let-syntax ((send (syntax-rules ()
                       ((_ option ...)
                        (for-each (lambda (i)
                                    (cond ((boolean? i)
                                           (write-int (if i 1 0) socket))
                                          ((integer? i)
                                           (write-int i socket))
                                          (else
                                           (error "invalid build option"
                                                  i))))
                                  (list option ...))))))
    (send (operation-id set-options)
          keep-failed? keep-going? try-fallback? verbosity
          max-build-jobs max-silent-time)
    (if (>= (nix-server-minor-version server) 2)
        (send use-build-hook?))
    (if (>= (nix-server-minor-version server) 4)
        (send build-verbosity log-type print-build-trace))
    (process-stderr server)))

(define-syntax define-operation
  (syntax-rules ()
    ((_ (name (type arg) ...) docstring return)
     (define (name server arg ...)
       docstring
       (let ((s (nix-server-socket server)))
         (write-int (operation-id name) s)
         (write-arg type arg s)
         ...
         (process-stderr server)
         (read-arg return s))))))

(define-operation (add-text-to-store (string name) (string text)
                                     (string-list references))
  "Add TEXT under file NAME in the store."
  store-path)

(define-operation (add-to-store (string basename)
                                (integer algo)
                                (boolean sha256-and-recursive?)
                                (boolean recursive?)
                                (file file-name))
  "Add the contents of FILE-NAME under BASENAME to the store."
  store-path)

(define-operation (build-derivations (string-list derivations))
  "Build DERIVATIONS; return #t on success."
  boolean)
