;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (guix avahi)
  #:use-module (guix records)
  #:use-module (guix build syscalls)
  #:use-module (avahi)
  #:use-module (avahi client)
  #:use-module (avahi client lookup)
  #:use-module (avahi client publish)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 threads)
  #:export (avahi-service
            avahi-service?
            avahi-service-name
            avahi-service-type
            avahi-service-interface
            avahi-service-local-address
            avahi-service-address
            avahi-service-port
            avahi-service-txt

            avahi-publish-service-thread
            avahi-browse-service-thread))

(define-record-type* <avahi-service>
  avahi-service make-avahi-service
  avahi-service?
  (name avahi-service-name)
  (type avahi-service-type)
  (interface avahi-service-interface)
  (local-address avahi-service-local-address)
  (address avahi-service-address)
  (port avahi-service-port)
  (txt avahi-service-txt))

(define* (avahi-publish-service-thread name
                                       #:key
                                       type port
                                       (stop-loop? (const #f))
                                       (timeout 100)
                                       (txt '()))
  "Publish the service TYPE using Avahi, for the given PORT, on all interfaces
and for all protocols. Also, advertise the given TXT record list.

This procedure starts a new thread running the Avahi event loop.  It exits
when STOP-LOOP? procedure returns true."
  (define client-callback
    (lambda (client state)
      (when (eq? state client-state/s-running)
        (let ((group (make-entry-group client (const #t))))
          (apply
           add-entry-group-service! group interface/unspecified
           protocol/unspecified '()
           name type #f #f port txt)
          (commit-entry-group group)))))

  (call-with-new-thread
   (lambda ()
     (let* ((poll (make-simple-poll))
            (client (make-client (simple-poll poll)
                                 (list
                                  client-flag/ignore-user-config)
                                 client-callback)))
       (while (not (stop-loop?))
         (iterate-simple-poll poll timeout))))))

(define (interface->ip-address interface)
  "Return the local IP address of the given INTERFACE."
  (let* ((socket (socket AF_INET SOCK_STREAM 0))
         (address (network-interface-address socket interface))
         (ip (inet-ntop (sockaddr:fam address)
                        (sockaddr:addr address))))
    (close-port socket)
    ip))

(define* (avahi-browse-service-thread proc
                                      #:key
                                      types
                                      (ignore-local? #t)
                                      (family AF_INET)
                                      (stop-loop? (const #f))
                                      (timeout 100))
  "Browse services which type is part of the TYPES list, using Avahi.  The
search is restricted to services with the given FAMILY.  Each time a service
is found or removed, PROC is called and passed as argument the corresponding
AVAHI-SERVICE record.  If a service is available on multiple network
interfaces, it will only be reported on the first interface found.

This procedure starts a new thread running the Avahi event loop.  It exits
when STOP-LOOP? procedure returns true."
  (define %known-hosts
    ;; Set of Avahi discovered hosts.
    (make-hash-table))

  (define (service-resolver-callback resolver interface protocol event
                                     service-name service-type domain
                                     host-name address-type address port
                                     txt flags)
    ;; Handle service resolution events.
    (cond ((eq? event resolver-event/found)
           ;; Add the service if the host is unknown.  This means that if a
           ;; service is available on multiple network interfaces for a single
           ;; host, only the first interface found will be considered.
           (unless (or (hash-ref %known-hosts service-name)
                       (and ignore-local?
                            (member lookup-result-flag/local flags)))
             (let* ((address (inet-ntop family address))
                    (local-address (interface->ip-address interface))
                    (service* (avahi-service
                               (name service-name)
                               (type service-type)
                               (interface interface)
                               (local-address local-address)
                               (address address)
                               (port port)
                               (txt txt))))
               (hash-set! %known-hosts service-name service*)
               (proc 'new-service service*)))))
    (free-service-resolver! resolver))

  (define (service-browser-callback browser interface protocol event
                                    service-name service-type
                                    domain flags)
    (cond
     ((eq? event browser-event/new)
      (make-service-resolver (service-browser-client browser)
                             interface protocol
                             service-name service-type domain
                             protocol/unspecified '()
                             service-resolver-callback))
     ((eq? event browser-event/remove)
      (let ((service (hash-ref %known-hosts service-name)))
        (when service
          (proc 'remove-service service)
          (hash-remove! %known-hosts service-name))))))

  (define client-callback
    (lambda (client state)
      (if (eq? state client-state/s-running)
          (for-each (lambda (type)
                      (make-service-browser client
                                            interface/unspecified
                                            protocol/inet
                                            type #f '()
                                            service-browser-callback))
                    types))))

  (let* ((poll (make-simple-poll))
         (client (make-client (simple-poll poll)
                              '() ;; no flags
                              client-callback)))
    (and (client? client)
         (while (not (stop-loop?))
           (iterate-simple-poll poll timeout)))))
