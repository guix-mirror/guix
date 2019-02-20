;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer connman)
  #:use-module (gnu installer utils)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (<technology>
            technology
            technology?
            technology-name
            technology-type
            technology-powered?
            technology-connected?

            <service>
            service
            service?
            service-name
            service-type
            service-path
            service-strength
            service-state

            &connman-error
            connman-error?
            connman-error-command
            connman-error-output
            connman-error-status

            &connman-connection-error
            connman-connection-error?
            connman-connection-error-service
            connman-connection-error-output

            &connman-password-error
            connman-password-error?

            &connman-already-connected-error
            connman-already-connected-error?

            connman-state
            connman-technologies
            connman-enable-technology
            connman-disable-technology
            connman-scan-technology
            connman-services
            connman-connect
            connman-disconnect
            connman-online?
            connman-connect-with-auth))

;;; Commentary:
;;;
;;; This module provides procedures for talking with the connman daemon.
;;; The best approach would have been using connman dbus interface.
;;; However, as Guile dbus bindings are not available yet, the console client
;;; "connmanctl" is used to talk with the daemon.
;;;


;;;
;;; Technology record.
;;;

;; The <technology> record encapsulates the "Technology" object of connman.
;; Technology type will be typically "ethernet", "wifi" or "bluetooth".

(define-record-type* <technology>
  technology make-technology
  technology?
  (name            technology-name) ; string
  (type            technology-type) ; string
  (powered?        technology-powered?) ; boolean
  (connected?      technology-connected?)) ; boolean


;;;
;;; Service record.
;;;

;; The <service> record encapsulates the "Service" object of connman.
;; Service type is the same as the technology it is associated to, path is a
;; unique identifier given by connman, strength describes the signal quality
;; if applicable. Finally, state is "idle", "failure", "association",
;; "configuration", "ready", "disconnect" or "online".

(define-record-type* <service>
  service make-service
  service?
  (name            service-name) ; string
  (type            service-type) ; string
  (path            service-path) ; string
  (strength        service-strength) ; integer
  (state           service-state)) ; string


;;;
;;; Condition types.
;;;

(define-condition-type &connman-error &error
  connman-error?
  (command connman-error-command)
  (output connman-error-output)
  (status connman-error-status))

(define-condition-type &connman-connection-error &error
  connman-connection-error?
  (service connman-connection-error-service)
  (output  connman-connection-error-output))

(define-condition-type &connman-password-error &connman-connection-error
  connman-password-error?)

(define-condition-type &connman-already-connected-error
  &connman-connection-error connman-already-connected-error?)


;;;
;;; Procedures.
;;;

(define (connman-run command env arguments)
  "Run the given COMMAND, with the specified ENV and ARGUMENTS.  The error
output is discarded and &connman-error condition is raised if the command
returns a non zero exit code."
  (let* ((command `("env" ,env ,command ,@arguments "2>" "/dev/null"))
         (command-string (string-join command " "))
         (pipe (open-input-pipe command-string))
         (output (read-lines pipe))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) output)
      (else (raise (condition (&connman-error
                               (command command)
                               (output output)
                               (status ret))))))))

(define (connman . arguments)
  "Run connmanctl with the specified ARGUMENTS. Set the LANG environment
variable to C because the command output will be parsed and we don't want it
to be translated."
  (connman-run "connmanctl" "LANG=C" arguments))

(define (parse-keys keys)
  "Parse the given list of strings KEYS, under the following format:

     '((\"KEY = VALUE\") (\"KEY2 = VALUE2\") ...)

Return the corresponding association list of '((KEY . VALUE) (KEY2 . VALUE2)
...)  elements."
  (let ((key-regex (make-regexp "([^ ]+) = ([^$]+)")))
    (map (lambda (key)
           (let ((match-key (regexp-exec key-regex key)))
             (cons (match:substring match-key 1)
                   (match:substring match-key 2))))
         keys)))

(define (connman-state)
  "Return the state of connman. The nominal states are 'offline, 'idle,
'ready, 'oneline.  If an unexpected state is read, 'unknown is
returned. Finally, an error is raised if the comman output could not be
parsed, usually because the connman daemon is not responding."
  (let* ((output (connman "state"))
         (state-keys (parse-keys output)))
    (let ((state (assoc-ref state-keys "State")))
      (if state
          (cond ((string=? state "offline") 'offline)
                ((string=? state "idle") 'idle)
                ((string=? state "ready") 'ready)
                ((string=? state "online") 'online)
                (else 'unknown))
          (raise (condition
                  (&message
                   (message "Could not determine the state of connman."))))))))

(define (split-technology-list technologies)
  "Parse the given strings list TECHNOLOGIES, under the following format:

	'((\"/net/connman/technology/xxx\")
          (\"KEY = VALUE\")
          ...
          (\"/net/connman/technology/yyy\")
          (\"KEY2 = VALUE2\")
          ...)
 Return the corresponding '(((\"KEY = VALUE\") ...) ((\"KEY2 = VALUE2\") ...))
list so that each keys of a given technology are gathered in a separate list."
  (let loop ((result '())
             (cur-list '())
             (input (reverse technologies)))
    (if (null? input)
        result
        (let ((item (car input)))
          (if (string-match "/net/connman/technology" item)
              (loop (cons cur-list result) '() (cdr input))
              (loop result (cons item cur-list) (cdr input)))))))

(define (string->boolean string)
  (equal? string "True"))

(define (connman-technologies)
  "Return a list of available <technology> records."

  (define (technology-output->technology output)
    (let ((keys (parse-keys output)))
      (technology
       (name (assoc-ref keys "Name"))
       (type (assoc-ref keys "Type"))
       (powered? (string->boolean (assoc-ref keys "Powered")))
       (connected? (string->boolean (assoc-ref keys "Connected"))))))

  (let* ((output (connman "technologies"))
         (technologies (split-technology-list output)))
    (map technology-output->technology technologies)))

(define (connman-enable-technology technology)
  "Enable the given TECHNOLOGY."
  (let ((type (technology-type technology)))
    (connman "enable" type)))

(define (connman-disable-technology technology)
  "Disable the given TECHNOLOGY."
  (let ((type (technology-type technology)))
    (connman "disable" type)))

(define (connman-scan-technology technology)
  "Run a scan for the given TECHNOLOGY."
  (let ((type (technology-type technology)))
    (connman "scan" type)))

(define (connman-services)
  "Return a list of available <services> records."

  (define (service-output->service path output)
    (let* ((service-keys
            (match output
              ((_ . rest) rest)))
           (keys (parse-keys service-keys)))
      (service
       (name (assoc-ref keys "Name"))
       (type (assoc-ref keys "Type"))
       (path path)
       (strength (and=> (assoc-ref keys "Strength") string->number))
       (state (assoc-ref keys "State")))))

  (let* ((out (connman "services"))
         (out-filtered (delete "" out))
         (services-path (map (lambda (service)
                               (match (string-split service #\ )
                                 ((_ ... path) path)))
                             out-filtered))
         (services-output (map (lambda (service)
                                 (connman "services" service))
                               services-path)))
    (map service-output->service services-path services-output)))

(define (connman-connect service)
  "Connect to the given SERVICE."
  (let ((path (service-path service)))
    (connman "connect" path)))

(define (connman-disconnect service)
  "Disconnect from the given SERVICE."
  (let ((path (service-path service)))
    (connman "disconnect" path)))

(define (connman-online?)
  (let ((state (connman-state)))
    (eq? state 'online)))

(define (connman-connect-with-auth service password-proc)
  "Connect to the given SERVICE with the password returned by calling
PASSWORD-PROC. This is only possible in the interactive mode of connmanctl
because authentication is done by communicating with an agent.

As the open-pipe procedure of Guile do not allow to read from stderr, we have
to merge stdout and stderr using bash redirection. Then error messages are
extracted from connmanctl output using a regexp. This makes the whole
procedure even more unreliable.

Raise &connman-connection-error if an error occurred during connection. Raise
&connman-password-error if the given password is incorrect."

  (define connman-error-regexp (make-regexp "Error[ ]*([^\n]+)\n"))

  (define (match-connman-error str)
    (let ((match-error (regexp-exec connman-error-regexp str)))
      (and match-error (match:substring match-error 1))))

  (define* (read-regexps-or-error port regexps error-handler)
    "Read characters from port until an error is detected, or one of the given
REGEXPS is matched. If an error is detected, call ERROR-HANDLER with the error
string as argument. Raise an error if the eof is reached before one of the
regexps is matched."
    (let loop ((res ""))
      (let ((char (read-char port)))
        (cond
         ((eof-object? char)
          (raise (condition
                  (&message
                   (message "Unable to find expected regexp.")))))
         ((match-connman-error res)
          =>
          (lambda (match)
            (error-handler match)))
         ((or-map (lambda (regexp)
                    (and (regexp-exec regexp res) regexp))
                  regexps)
          =>
          (lambda (match)
            match))
         (else
          (loop (string-append res (string char))))))))

  (define* (read-regexp-or-error port regexp error-handler)
    "Same as READ-REGEXPS-OR-ERROR above, but with a single REGEXP."
    (read-regexps-or-error port (list regexp) error-handler))

  (define (connman-error->condition path error)
    (cond
     ((string-match "Already connected" error)
      (condition (&connman-already-connected-error
                  (service path)
                  (output error))))
     (else
      (condition (&connman-connection-error
                  (service path)
                  (output error))))))

  (define (run-connection-sequence pipe)
    "Run the connection sequence using PIPE as an opened port to an
interactive connmanctl process."
    (let* ((path (service-path service))
           (error-handler (lambda (error)
                            (raise
                             (connman-error->condition path error)))))
      ;; Start the agent.
      (format pipe "agent on\n")
      (read-regexp-or-error pipe (make-regexp "Agent registered") error-handler)

      ;; Let's try to connect to the service. If the service does not require
      ;; a password, the connection might succeed right after this call.
      ;; Otherwise, connmanctl will prompt us for a password.
      (format pipe "connect ~a\n" path)
      (let* ((connected-regexp (make-regexp (format #f "Connected ~a" path)))
             (passphrase-regexp (make-regexp "\nPassphrase\\?[ ]*"))
             (regexps (list connected-regexp passphrase-regexp))
             (result (read-regexps-or-error pipe regexps error-handler)))

        ;; A password is required.
        (when (eq? result passphrase-regexp)
          (format pipe "~a~%" (password-proc))

          ;; Now, we have to wait for the connection to succeed. If an error
          ;; occurs, it is most likely because the password is incorrect.
          ;; In that case, we escape from an eventual retry loop that would
          ;; add complexity to this procedure, and raise a
          ;; &connman-password-error condition.
          (read-regexp-or-error pipe connected-regexp
                                (lambda (error)
                                  ;; Escape from retry loop.
                                  (format pipe "no\n")
                                  (raise
                                   (condition (&connman-password-error
                                               (service path)
                                               (output error))))))))))

  ;; XXX: Find a better way to read stderr, like with the "subprocess"
  ;; procedure of racket that return input ports piped on the process stdin and
  ;; stderr.
  (let ((pipe (open-pipe "connmanctl 2>&1" OPEN_BOTH)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (run-connection-sequence pipe)
        #t)
      (lambda ()
        (format pipe "quit\n")
        (close-pipe pipe)))))
