;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu services herd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (%shepherd-socket-file
            shepherd-message-port

            shepherd-error?
            service-not-found-error?
            service-not-found-error-service
            action-not-found-error?
            action-not-found-error-service
            action-not-found-error-action
            action-exception-error?
            action-exception-error-service
            action-exception-error-action
            action-exception-error-key
            action-exception-error-arguments
            unknown-shepherd-error?
            unknown-shepherd-error-sexp

            live-service
            live-service?
            live-service-provision
            live-service-requirement
            live-service-running
            live-service-canonical-name

            with-shepherd-action
            current-services
            unload-services
            unload-service
            load-services
            load-services/safe
            start-service
            stop-service
            restart-service))

;;; Commentary:
;;;
;;; This module provides an interface to the GNU Shepherd, similar to the
;;; 'herd' command.  Essentially it implements a subset of the (shepherd comm)
;;; module, but focusing only on the parts relevant to 'guix system
;;; reconfigure'.
;;;
;;; Code:

(define %shepherd-socket-file
  (make-parameter "/var/run/shepherd/socket"))

(define* (open-connection #:optional (file (%shepherd-socket-file)))
  "Open a connection to the daemon, using the Unix-domain socket at FILE, and
return the socket."
  ;; The protocol is sexp-based and UTF-8-encoded.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (let ((sock    (socket PF_UNIX SOCK_STREAM 0))
          (address (make-socket-address PF_UNIX file)))
      (catch 'system-error
        (lambda ()
          (connect sock address)
          (setvbuf sock 'block 1024)
          sock)
        (lambda args
          (close-port sock)
          (apply throw args))))))

(define-syntax-rule (with-shepherd connection body ...)
  "Evaluate BODY... with CONNECTION bound to an open socket to PID 1."
  (let ((connection (open-connection)))
    (dynamic-wind
      (const #t)
      (lambda ()
        body ...)
      (lambda ()
        (close-port connection)))))

(define-condition-type &shepherd-error &error
  shepherd-error?)

(define-condition-type &service-not-found-error &shepherd-error
  service-not-found-error?
  (service service-not-found-error-service))

(define-condition-type &action-not-found-error &shepherd-error
  action-not-found-error?
  (service action-not-found-error-service)
  (action  action-not-found-error-action))

(define-condition-type &action-exception-error &shepherd-error
  action-exception-error?
  (service action-exception-error-service)
  (action  action-exception-error-action)
  (key     action-exception-error-key)
  (args    action-exception-error-arguments))

(define-condition-type &unknown-shepherd-error &shepherd-error
  unknown-shepherd-error?
  (sexp   unknown-shepherd-error-sexp))

(define (raise-shepherd-error error)
  "Raise an error condition corresponding to ERROR, an sexp received by a
shepherd client in reply to COMMAND, a command object.  Return #t if ERROR
does not denote an error."
  (match error
    (('error ('version 0 x ...) 'service-not-found service)
     (raise (condition (&service-not-found-error
                        (service service)))))
    (('error ('version 0 x ...) 'action-not-found action service)
     (raise (condition (&action-not-found-error
                        (service service)
                        (action action)))))
    (('error ('version 0 x ...) 'action-exception action service
             key (args ...))
     (raise (condition (&action-exception-error
                        (service service)
                        (action action)
                        (key key) (args args)))))
    (('error . _)
     (raise (condition (&unknown-shepherd-error (sexp error)))))
    (#f                                           ;not an error
     #t)))

(define shepherd-message-port
  ;; Port where messages coming from shepherd are printed.
  (make-parameter (current-error-port)))

(define (display-message message)
  (format (shepherd-message-port) "shepherd: ~a~%" message))

(define* (invoke-action service action arguments cont)
  "Invoke ACTION on SERVICE with ARGUMENTS.  On success, call CONT with the
list of results (one result per instance with the name SERVICE).  Otherwise
return #f."
  (with-shepherd sock
    (write `(shepherd-command (version 0)
                              (action ,action)
                              (service ,service)
                              (arguments ,arguments)
                              (directory ,(getcwd)))
           sock)
    (force-output sock)

    (match (read sock)
      (('reply ('version 0 _ ...) ('result result) ('error #f)
               ('messages messages))
       (for-each display-message messages)
       (cont result))
      (('reply ('version 0 x ...) ('result y) ('error error)
               ('messages messages))
       (for-each display-message messages)
       (raise-shepherd-error error)
       #f)
      (x
       ;; invalid reply
       #f))))

(define-syntax-rule (with-shepherd-action service (action args ...)
                      result body ...)
  "Invoke ACTION on SERVICE with the given ARGS, and evaluate BODY with RESULT
bound to the action's result."
  (invoke-action service action (list args ...)
                 (lambda (result) body ...)))

(define-syntax alist-let*
  (syntax-rules ()
    "Bind the given KEYs in EXP to the corresponding items in ALIST.  ALIST
is assumed to be a list of two-element tuples rather than a traditional list
of pairs."
    ((_ alist (key ...) exp ...)
     (let ((key (and=> (assoc-ref alist 'key) car)) ...)
       exp ...))))

;; Information about live Shepherd services.
(define-record-type <live-service>
  (live-service provision requirement running)
  live-service?
  (provision    live-service-provision)           ;list of symbols
  (requirement  live-service-requirement)         ;list of symbols
  (running      live-service-running))            ;#f | object

(define (live-service-canonical-name service)
  "Return the 'canonical name' of SERVICE."
  (first (live-service-provision service)))

(define (current-services)
  "Return the list of currently defined Shepherd services, represented as
<live-service> objects.  Return #f if the list of services could not be
obtained."
  (with-shepherd-action 'root ('status) results
    ;; We get a list of results, one for each service with the name 'root'.
    ;; In practice there's only one such service though.
    (match results
      ((services _ ...)
       (match services
         ((('service ('version 0 _ ...) _ ...) ...)
          (map (lambda (service)
                 (alist-let* service (provides requires running)
                   (live-service provides requires running)))
               services))
         (x
          #f))))))

(define (unload-service service)
  "Unload SERVICE, a symbol name; return #t on success."
  (with-shepherd-action 'root ('unload (symbol->string service)) result
    (first result)))

(define (%load-file file)
  "Load FILE in the Shepherd."
  (with-shepherd-action 'root ('load file) result
    (first result)))

(define (eval-there exp)
  "Eval EXP in the Shepherd."
  (with-shepherd-action 'root ('eval (object->string exp)) result
    (first result)))

(define (load-services files)
  "Load and register the services from FILES, where FILES contain code that
returns a shepherd <service> object."
  (eval-there `(register-services
                ,@(map (lambda (file)
                         `(primitive-load ,file))
                       files))))

(define (load-services/safe files)
  "This is like 'load-services', but make sure only the subset of FILES that
can be safely reloaded is actually reloaded.

This is done to accommodate the Shepherd < 0.15.0 where services lacked the
'replacement' slot, and where 'register-services' would throw an exception
when passed a service with an already-registered name."
  (eval-there `(let* ((services     (map primitive-load ',files))
                      (slots        (map slot-definition-name
                                         (class-slots <service>)))
                      (can-replace? (memq 'replacement slots)))
                 (define (registered? service)
                   (not (null? (lookup-services (canonical-name service)))))

                 (apply register-services
                        (if can-replace?
                            services
                            (remove registered? services))))))

(define* (start-service name #:optional (arguments '()))
  (invoke-action name 'start arguments
                 (lambda (result)
                   result)))

(define (stop-service name)
  (with-shepherd-action name ('stop) result
    result))

(define (restart-service name)
  (with-shepherd-action name ('restart) result
    result))

;; Local Variables:
;; eval: (put 'alist-let* 'scheme-indent-function 2)
;; eval: (put 'with-shepherd 'scheme-indent-function 1)
;; eval: (put 'with-shepherd-action 'scheme-indent-function 3)
;; End:

;;; herd.scm ends here
