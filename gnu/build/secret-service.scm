;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu build secret-service)
  #:use-module (guix build utils)

  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)

  #:export (secret-service-receive-secrets
            secret-service-send-secrets))

;;; Commentary:
;;;
;;; Utility procedures for copying secrets into a VM.
;;;
;;; Code:

(define* (secret-service-send-secrets port secret-root #:key (retry 60))
  "Copy all files under SECRET-ROOT using TCP to secret-service listening at
local PORT.  If connect fails, sleep 1s and retry RETRY times."

  (define (file->file+size+mode file-name)
    (let ((stat (stat file-name))
          (target (substring file-name (string-length secret-root))))
      (list target (stat:size stat) (stat:mode stat))))

  (format (current-error-port) "sending secrets to ~a~%" port)
  (let ((sock (socket AF_INET SOCK_STREAM 0))
        (addr (make-socket-address AF_INET INADDR_LOOPBACK port)))
    ;; connect to wait for port
    (let loop ((retry retry))
      (catch 'system-error
        (cute connect sock addr)
        (lambda (key . args)
          (when (zero? retry)
            (apply throw key args))
          (format (current-error-port)
                  "secret service: retrying connection [~a attempts left]~%"
                  (- retry 1))
          (sleep 1)
          (loop (1- retry)))))

    (format (current-error-port)
            "secret service: connected; sending files in ~s~%"
            secret-root)
    (let* ((files (if secret-root (find-files secret-root) '()))
           (files-sizes-modes (map file->file+size+mode files))
           (secrets `(secrets
                      (version 0)
                      (files ,files-sizes-modes))))
      (write secrets sock)
      (for-each (lambda (file)
                  (call-with-input-file file
                    (lambda (input)
                      (dump-port input sock))))
                files))))

(define (secret-service-receive-secrets port)
  "Listen to local PORT and wait for a secret service client to send secrets.
Write them to the file system.  Return the list of files installed on success,
and #f otherwise."

  (define (wait-for-client port)
    ;; Wait for a TCP connection on PORT.  Note: We cannot use the
    ;; virtio-serial ports, which would be safer, because they are
    ;; (presumably) unsupported on GNU/Hurd.
    (let ((sock (socket AF_INET SOCK_STREAM 0)))
      (bind sock AF_INET INADDR_ANY port)
      (listen sock 1)
      (format (current-error-port)
              "secret service: waiting for secrets on port ~a...~%"
              port)
      (match (select (list sock) '() '() 60)
        (((_) () ())
         (match (accept sock)
           ((client . address)
            (format (current-error-port)
                    "secret service: client connection from ~a~%"
                    (inet-ntop (sockaddr:fam address)
                               (sockaddr:addr address)))
            (close-port sock)
            client)))
        ((() () ())
         (format (current-error-port)
                 "secret service: did not receive any secrets; time out~%")
         #f))))

  ;; TODO: Remove when (@ (guix build utils) dump-port) has a 'size'
  ;; parameter.
  (define (dump in out size)
    ;; Copy SIZE bytes from IN to OUT.
    (define buf-size 65536)
    (define buf (make-bytevector buf-size))

    (let loop ((left size))
      (if (<= left 0)
          0
          (let ((read (get-bytevector-n! in buf 0 (min left buf-size))))
            (if (eof-object? read)
                left
                (begin
                  (put-bytevector out buf 0 read)
                  (loop (- left read))))))))

  (define (read-secrets port)
    ;; Read secret files from PORT and install them.
    (match (false-if-exception (read port))
      (('secrets ('version 0)
                 ('files ((files sizes modes) ...)))
       (for-each (lambda (file size mode)
                   (format (current-error-port)
                           "secret service: \
installing file '~a' (~a bytes)...~%"
                           file size)
                   (mkdir-p (dirname file))
                   (call-with-output-file file
                     (lambda (output)
                       (dump port output size)
                       (chmod file mode))))
                 files sizes modes)
       files)
      (_
       (format (current-error-port)
               "secret service: invalid secrets received~%")
       #f)))

  (let* ((port   (wait-for-client port))
         (result (and=> port read-secrets)))
    (when port
      (close-port port))
    result))

;;; secret-service.scm ends here
