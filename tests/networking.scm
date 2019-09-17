;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (tests networking)
  #:use-module (ice-9 regex)
  #:use-module (gnu services networking)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services networking) module.

(test-begin "networking")


;;;
;;; NTP.
;;;

(define ntp-server->string (@@ (gnu services networking) ntp-server->string))

(define %ntp-server-sample
  (ntp-server
   (type 'server)
   (address "some.ntp.server.org")
   (options `(iburst (version 3) (maxpoll 16) prefer))))

(test-equal "ntp-server->string"
  (ntp-server->string %ntp-server-sample)
  "server some.ntp.server.org iburst version 3 maxpoll 16 prefer")

(test-equal "ntp configuration servers deprecated form"
  (ntp-configuration-servers
   (ntp-configuration
    (servers (list (ntp-server
                    (type 'server)
                    (address "example.pool.ntp.org")
                    (options '()))))))
  (ntp-configuration-servers
   (ntp-configuration
    (servers (list "example.pool.ntp.org")))))


;;;
;;; OpenNTPD
;;;

(define openntpd-configuration->string (@@ (gnu services networking)
                                           openntpd-configuration->string))

(define %openntpd-conf-sample
  (openntpd-configuration
   (server '("0.guix.pool.ntp.org" "1.guix.pool.ntp.org"))
   (listen-on '("127.0.0.1" "::1"))
   (sensor '("udcf0 correction 70000"))
   (constraint-from '("www.gnu.org"))
   (constraints-from '("https://www.google.com/"))
   (allow-large-adjustment? #t)))

(test-assert "openntpd configuration generation sanity check"

  (begin
    (define (string-match/newline pattern text)
      (regexp-exec (make-regexp pattern regexp/newline) text))

    (define (match-count pattern text)
      (fold-matches (make-regexp pattern regexp/newline) text 0
                    (lambda (match count)
                      (1+ count))))

    (let ((config (openntpd-configuration->string %openntpd-conf-sample)))
      (if (not
           (and (string-match/newline "^listen on 127.0.0.1$" config)
                (string-match/newline "^listen on ::1$" config)
                (string-match/newline "^sensor udcf0 correction 70000$" config)
                (string-match/newline "^constraint from www.gnu.org$" config)
                (string-match/newline "^server 0.guix.pool.ntp.org$" config)
                (string-match/newline
                 "^constraints from \"https://www.google.com/\"$"
                 config)

                ;; Check for issue #3731 (see:
                ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=37318).
                (= (match-count "^listen on " config) 2)
                (= (match-count "^sensor " config) 1)
                (= (match-count "^constraint from " config) 1)
                (= (match-count "^server " config) 2)
                (= (match-count "^constraints from " config) 1)))
          (begin
            (format #t "The configuration below failed \
the sanity check:\n~a~%" config)
            #f)
          #t))))

(test-equal "openntpd generated config string ends with a newline"
  (let ((config (openntpd-configuration->string %openntpd-conf-sample)))
    (string-take-right config 1))
  "\n")

(test-end "networking")
