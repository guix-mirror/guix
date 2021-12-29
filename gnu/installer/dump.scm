;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu installer dump)
  #:use-module (gnu installer utils)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web response)
  #:use-module (webutils multipart)
  #:export (make-dump
            send-dump-report))

;; The installer crash dump type.
(define %dump-type "installer-dump")

(define (result->list result)
  "Return the alist for the given RESULT."
  (hash-map->list (lambda (k v)
                    (cons k v))
                  result))

(define* (make-dump output
                    #:key
                    result
                    backtrace)
  "Create a crash dump archive in OUTPUT.  RESULT is the installer result hash
table.  BACKTRACE is the installer Guile backtrace."
  (let ((dump-dir "/tmp/dump"))
    (mkdir-p dump-dir)
    (with-directory-excursion dump-dir
      ;; backtrace
      (copy-file backtrace "installer-backtrace")

      ;; installer result
      (call-with-output-file "installer-result"
        (lambda (port)
          (write (result->list result) port)))

      ;; syslog
      (copy-file "/var/log/messages" "syslog")

      ;; dmesg
      (let ((pipe (open-pipe* OPEN_READ "dmesg")))
        (call-with-output-file "dmesg"
          (lambda (port)
            (dump-port pipe port)
            (close-pipe pipe)))))

    (with-directory-excursion (dirname dump-dir)
      (system* "tar" "-zcf" output (basename dump-dir)))))

(define* (send-dump-report dump
                           #:key
                           (url "https://dump.guix.gnu.org"))
  "Turn the DUMP archive into a multipart body and send it to the Guix crash
dump server at URL."
  (define (match-boundary kont)
    (match-lambda
      (('boundary . (? string? b))
       (kont b))
      (x #f)))

  (define (response->string response)
    (bytevector->string
     (read-response-body response)
     "UTF-8"))

  (let-values (((body boundary)
                (call-with-input-file dump
                  (lambda (port)
                    (format-multipart-body
                     `((,%dump-type . ,port)))))))
    (false-if-exception
     (response->string
      (http-post
       (string-append url "/upload")
       #:keep-alive? #t
       #:streaming? #t
       #:headers `((content-type
                    . (multipart/form-data
                       (boundary . ,boundary))))
       #:body body)))))
