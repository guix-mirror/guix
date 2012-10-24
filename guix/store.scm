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
  #:use-module (guix utils)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:export (nix-server?
            nix-server-major-version
            nix-server-minor-version
            nix-server-socket

            &nix-error nix-error?
            &nix-protocol-error nix-protocol-error?
            nix-protocol-error-message
            nix-protocol-error-status

            hash-algo

            open-connection
            close-connection
            set-build-options
            valid-path?
            query-path-hash
            add-text-to-store
            add-to-store
            build-derivations

            current-build-output-port

            %store-prefix
            store-path?
            derivation-path?))

(define %protocol-version #x10b)

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

  (write-string %archive-version-1 p)

  (let dump ((f f))
    (let ((s (lstat f)))
      (write-string "(" p)
      (case (stat:type s)
        ((regular)
         (write-string "type" p)
         (write-string "regular" p)
         (if (not (zero? (logand (stat:mode s) #o100)))
             (begin
               (write-string "executable" p)
               (write-string "" p)))
         (write-contents f p))
        ((directory)
         (write-string "type" p)
         (write-string "directory" p)
         (let ((entries (remove (cut member <> '("." ".."))
                                (scandir f))))
           (for-each (lambda (e)
                       (let ((f (string-append f "/" e)))
                         (write-string "entry" p)
                         (write-string "(" p)
                         (write-string "name" p)
                         (write-string e p)
                         (write-string "node" p)
                         (dump f)
                         (write-string ")" p)))
                     entries)))
        (else
         (error "ENOSYS")))
      (write-string ")" p))))

(define-syntax write-arg
  (syntax-rules (integer boolean file string string-list base16)
    ((_ integer arg p)
     (write-int arg p))
    ((_ boolean arg p)
     (write-int (if arg 1 0) p))
    ((_ file arg p)
     (write-file arg p))
    ((_ string arg p)
     (write-string arg p))
    ((_ string-list arg p)
     (write-string-list arg p))
    ((_ base16 arg p)
     (write-string (bytevector->base16-string arg) p))))

(define-syntax read-arg
  (syntax-rules (integer boolean string store-path base16)
    ((_ integer p)
     (read-int p))
    ((_ boolean p)
     (not (zero? (read-int p))))
    ((_ string p)
     (read-string p))
    ((_ store-path p)
     (read-store-path p))
    ((_ hash p)
     (base16-string->bytevector (read-string p)))))


;; remote-store.cc

(define-record-type <nix-server>
  (%make-nix-server socket major minor)
  nix-server?
  (socket nix-server-socket)
  (major  nix-server-major-version)
  (minor  nix-server-minor-version))

(define-condition-type &nix-error &error
  nix-error?)

(define-condition-type &nix-protocol-error &nix-error
  nix-protocol-error?
  (message nix-protocol-error-message)
  (status  nix-protocol-error-status))

(define* (open-connection #:optional (file %default-socket-path)
                          #:key (reserve-space? #t))
  (let ((s (with-fluids ((%default-port-encoding #f))
             ;; This trick allows use of the `scm_c_read' optimization.
             (socket PF_UNIX SOCK_STREAM 0)))
        (a (make-socket-address PF_UNIX file)))

    ;; Enlarge the receive buffer.
    (setsockopt s SOL_SOCKET SO_RCVBUF (* 12 1024))

    (connect s a)
    (write-int %worker-magic-1 s)
    (let ((r (read-int s)))
      (and (eqv? r %worker-magic-2)
           (let ((v (read-int s)))
             (and (eqv? (protocol-major %protocol-version)
                        (protocol-major v))
                  (begin
                    (write-int %protocol-version s)
                    (if (>= (protocol-minor v) 11)
                        (write-int (if reserve-space? 1 0) s))
                    (let ((s (%make-nix-server s
                                               (protocol-major v)
                                               (protocol-minor v))))
                      (process-stderr s)
                      s))))))))

(define (close-connection server)
  "Close the connection to SERVER."
  (close (nix-server-socket server)))

(define current-build-output-port
  ;; The port where build output is sent.
  (make-parameter (current-error-port)))

(define (process-stderr server)
  "Read standard output and standard error from SERVER, writing it to
CURRENT-BUILD-OUTPUT-PORT.  Return #t when SERVER is done sending data, and
#f otherwise; in the latter case, the caller should call `process-stderr'
again until #t is returned or an error is raised."
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
           (read-string p)
           #f)
          ((= k %stderr-read)
           (let ((len (read-int p)))
             (read-string p)                      ; FIXME: what to do?
             #f))
          ((= k %stderr-next)
           (let ((s (read-string p)))
             (display s (current-build-output-port))
             #f))
          ((= k %stderr-error)
           (let ((error  (read-string p))
                 (status (if (>= (nix-server-minor-version server) 8)
                             (read-int p)
                             1)))
             (raise (condition (&nix-protocol-error
                                (message error)
                                (status  status))))))
          ((= k %stderr-last)
           ;; The daemon is done (see `stopWork' in `nix-worker.cc'.)
           #t)
          (else
           (raise (condition (&nix-protocol-error
                              (message "invalid error code")
                              (status   k))))))))

(define* (set-build-options server
                            #:key keep-failed? keep-going? try-fallback?
                            (verbosity 0)
                            (max-build-jobs (current-processor-count))
                            (max-silent-time 3600)
                            (use-build-hook? #t)
                            (build-verbosity 0)
                            (log-type 0)
                            (print-build-trace #t)
                            (build-cores 1)
                            (use-substitutes? #t))
  ;; Must be called after `open-connection'.

  (define socket
    (nix-server-socket server))

  (let-syntax ((send (syntax-rules ()
                       ((_ (type option) ...)
                        (begin
                          (write-arg type option socket)
                          ...)))))
    (write-int (operation-id set-options) socket)
    (send (boolean keep-failed?) (boolean keep-going?)
          (boolean try-fallback?) (integer verbosity)
          (integer max-build-jobs) (integer max-silent-time))
    (if (>= (nix-server-minor-version server) 2)
        (send (boolean use-build-hook?)))
    (if (>= (nix-server-minor-version server) 4)
        (send (integer build-verbosity) (integer log-type)
              (boolean print-build-trace)))
    (if (>= (nix-server-minor-version server) 6)
        (send (integer build-cores)))
    (if (>= (nix-server-minor-version server) 10)
        (send (boolean use-substitutes?)))
    (let loop ((done? (process-stderr server)))
      (or done? (process-stderr server)))))

(define-syntax define-operation
  (syntax-rules ()
    ((_ (name (type arg) ...) docstring return)
     (define (name server arg ...)
       docstring
       (let ((s (nix-server-socket server)))
         (write-int (operation-id name) s)
         (write-arg type arg s)
         ...
         ;; Loop until the server is done sending error output.
         (let loop ((done? (process-stderr server)))
           (or done? (loop (process-stderr server))))
         (read-arg return s))))))

(define-operation (valid-path? (string path))
  "Return #t when PATH is a valid store path."
  boolean)

(define-operation (query-path-hash (string path))
  "Return the SHA256 hash of PATH as a bytevector."
  base16)

(define-operation (add-text-to-store (string name) (string text)
                                     (string-list references))
  "Add TEXT under file NAME in the store."
  store-path)

(define-operation (add-to-store (string basename)
                                (boolean fixed?)  ; obsolete, must be #t
                                (boolean recursive?)
                                (string hash-algo)
                                (file file-name))
  "Add the contents of FILE-NAME under BASENAME to the store."
  store-path)

(define-operation (build-derivations (string-list derivations))
  "Build DERIVATIONS, and return when the worker is done building them.
Return #t on success."
  boolean)


;;;
;;; Store paths.
;;;

(define %store-prefix
  ;; Absolute path to the Nix store.
  (make-parameter "/nix/store"))

(define (store-path? path)
  "Return #t if PATH is a store path."
  ;; This is a lightweight check, compared to using a regexp, but this has to
  ;; be fast as it's called often in `derivation', for instance.
  ;; `isStorePath' in Nix does something similar.
  (string-prefix? (%store-prefix) path))

(define (derivation-path? path)
  "Return #t if PATH is a derivation path."
  (and (store-path? path) (string-suffix? ".drv" path)))
