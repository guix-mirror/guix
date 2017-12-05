;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix ssh)
  #:use-module (guix store)
  #:use-module (guix i18n)
  #:use-module (ssh session)
  #:use-module (ssh auth)
  #:use-module (ssh key)
  #:use-module (ssh channel)
  #:use-module (ssh popen)
  #:use-module (ssh session)
  #:use-module (ssh dist)
  #:use-module (ssh dist node)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:export (open-ssh-session
            remote-daemon-channel
            connect-to-remote-daemon
            send-files
            retrieve-files
            remote-store-host

            file-retrieval-port))

;;; Commentary:
;;;
;;; This module provides tools to support communication with remote stores
;;; over SSH, using Guile-SSH.
;;;
;;; Code:

(define %compression
  "zlib@openssh.com,zlib")

(define* (open-ssh-session host #:key user port
                           (compression %compression))
  "Open an SSH session for HOST and return it.  When USER and PORT are #f, use
default values or whatever '~/.ssh/config' specifies; otherwise use them.
Throw an error on failure."
  (let ((session (make-session #:user user
                               #:host host
                               #:port port
                               #:timeout 10       ;seconds
                               ;; #:log-verbosity 'protocol

                               ;; We need lightweight compression when
                               ;; exchanging full archives.
                               #:compression compression
                               #:compression-level 3)))

    ;; Honor ~/.ssh/config.
    (session-parse-config! session)

    (match (connect! session)
      ('ok
       ;; Use public key authentication, via the SSH agent if it's available.
       (match (userauth-public-key/auto! session)
         ('success
          session)
         (x
          (disconnect! session)
          (raise (condition
                  (&message
                   (message (format #f (G_ "SSH authentication failed for '~a': ~a~%")
                                    host (get-error session)))))))))
      (x
       ;; Connection failed or timeout expired.
       (raise (condition
               (&message
                (message (format #f (G_ "SSH connection to '~a' failed: ~a~%")
                                 host (get-error session))))))))))

(define* (remote-daemon-channel session
                                #:optional
                                (socket-name
                                 "/var/guix/daemon-socket/socket"))
  "Return an input/output port (an SSH channel) to the daemon at SESSION."
  (define redirect
    ;; Code run in SESSION to redirect the remote process' stdin/stdout to the
    ;; daemon's socket, à la socat.  The SSH protocol supports forwarding to
    ;; Unix-domain sockets but libssh doesn't have an API for that, hence this
    ;; hack.
    `(begin
       (use-modules (ice-9 match) (rnrs io ports))

       (let ((sock   (socket AF_UNIX SOCK_STREAM 0))
             (stdin  (current-input-port))
             (stdout (current-output-port)))
         (setvbuf stdin _IONBF)
         (setvbuf stdout _IONBF)
         (connect sock AF_UNIX ,socket-name)

         (let loop ()
           (match (select (list stdin sock) '() (list stdin stdout sock))
             ((reads writes ())
              (when (memq stdin reads)
                (match (get-bytevector-some stdin)
                  ((? eof-object?)
                   (primitive-exit 0))
                  (bv
                   (put-bytevector sock bv))))
              (when (memq sock reads)
                (match (get-bytevector-some sock)
                  ((? eof-object?)
                   (primitive-exit 0))
                  (bv
                   (put-bytevector stdout bv))))
              (loop))
             (_
              (primitive-exit 1)))))))

  (open-remote-pipe* session OPEN_BOTH
                     ;; Sort-of shell-quote REDIRECT.
                     "guile" "-c"
                     (object->string
                      (object->string redirect))))

(define* (connect-to-remote-daemon session
                                   #:optional
                                   (socket-name
                                    "/var/guix/daemon-socket/socket"))
  "Connect to the remote build daemon listening on SOCKET-NAME over SESSION,
an SSH session.  Return a <nix-server> object."
  (open-connection #:port (remote-daemon-channel session)))


(define (store-import-channel session)
  "Return an output port to which archives to be exported to SESSION's store
can be written."
  ;; Using the 'import-paths' RPC on a remote store would be slow because it
  ;; makes a round trip every time 32 KiB have been transferred.  This
  ;; procedure instead opens a separate channel to use the remote
  ;; 'import-paths' procedure, which consumes all the data in a single round
  ;; trip.  This optimizes the successful case at the expense of error
  ;; conditions: errors can only be reported once all the input has been
  ;; consumed.
  (define import
    `(begin
       (use-modules (guix) (srfi srfi-34)
                    (rnrs io ports) (rnrs bytevectors))

       (define (consume-input port)
         (let ((bv (make-bytevector 32768)))
           (let loop ()
             (let ((n (get-bytevector-n! port bv 0
                                         (bytevector-length bv))))
               (unless (eof-object? n)
                 (loop))))))

       ;; Upon completion, write an sexp that denotes the status.
       (write
        (catch #t
          (lambda ()
            (guard (c ((nix-protocol-error? c)
                       ;; Consume all the input since the only time we can
                       ;; report the error is after everything has been
                       ;; consumed.
                       (consume-input (current-input-port))
                       (list 'protocol-error (nix-protocol-error-message c))))
              (with-store store
                (setvbuf (current-input-port) _IONBF)
                (import-paths store (current-input-port))
                '(success))))
          (lambda args
            (cons 'error args))))))

  (open-remote-pipe session
                    (string-join
                     `("guile" "-c"
                       ,(object->string (object->string import))))
                    OPEN_BOTH))

(define* (store-export-channel session files
                               #:key recursive?)
  "Return an input port from which an export of FILES from SESSION's store can
be read.  When RECURSIVE? is true, the closure of FILES is exported."
  ;; Same as above: this is more efficient than calling 'export-paths' on a
  ;; remote store.
  (define export
    `(begin
       (use-modules (guix))

       (with-store store
         (setvbuf (current-output-port) _IONBF)

         ;; FIXME: Exceptions are silently swallowed.  We should report them
         ;; somehow.
         (export-paths store ',files (current-output-port)
                       #:recursive? ,recursive?))))

  (open-remote-input-pipe session
                          (string-join
                           `("guile" "-c"
                             ,(object->string
                               (object->string export))))))

(define* (send-files local files remote
                     #:key
                     recursive?
                     (log-port (current-error-port)))
  "Send the subset of FILES from LOCAL (a local store) that's missing to
REMOTE, a remote store.  When RECURSIVE? is true, send the closure of FILES.
Return the list of store items actually sent."
  ;; Compute the subset of FILES missing on SESSION and send them.
  (let* ((files   (if recursive? (requisites local files) files))
         (session (channel-get-session (nix-server-socket remote)))
         (node    (make-node session))
         (missing (node-eval node
                             `(begin
                                (use-modules (guix)
                                             (srfi srfi-1) (srfi srfi-26))

                                (with-store store
                                  (remove (cut valid-path? store <>)
                                          ',files)))))
         (count   (length missing))
         (port    (store-import-channel session)))
    (format log-port (N_ "sending ~a store item to '~a'...~%"
                         "sending ~a store items to '~a'...~%" count)
            count (session-get session 'host))

    ;; Send MISSING in topological order.
    (export-paths local missing port)

    ;; Tell the remote process that we're done.  (In theory the end-of-archive
    ;; mark of 'export-paths' would be enough, but in practice it's not.)
    (channel-send-eof port)

    ;; Wait for completion of the remote process and read the status sexp from
    ;; PORT.
    (let* ((result (false-if-exception (read port)))
           (status (zero? (channel-get-exit-status port))))
      (close-port port)
      (match result
        (('success . _)
         missing)
        (('protocol-error message)
         (raise (condition
                 (&nix-protocol-error (message message) (status 42)))))
        (('error key args ...)
         (raise (condition
                 (&nix-protocol-error
                  (message (call-with-output-string
                             (lambda (port)
                               (print-exception port #f key args))))
                  (status 43)))))
        (_
         (raise (condition
                 (&nix-protocol-error
                  (message "unknown error while sending files over SSH")
                  (status 44)))))))))

(define (remote-store-session remote)
  "Return the SSH channel beneath REMOTE, a remote store as returned by
'connect-to-remote-daemon', or #f."
  (channel-get-session (nix-server-socket remote)))

(define (remote-store-host remote)
  "Return the name of the host REMOTE is connected to, where REMOTE is a
remote store as returned by 'connect-to-remote-daemon'."
  (match (remote-store-session remote)
    (#f #f)
    ((? session? session)
     (session-get session 'host))))

(define* (file-retrieval-port files remote
                              #:key recursive?)
  "Return an input port from which to retrieve FILES (a list of store items)
from REMOTE, along with the number of items to retrieve (lower than or equal
to the length of FILES.)"
  (values (store-export-channel (remote-store-session remote) files
                                #:recursive? recursive?)
          (length files)))            ;XXX: inaccurate when RECURSIVE? is true

(define* (retrieve-files local files remote
                         #:key recursive? (log-port (current-error-port)))
  "Retrieve FILES from REMOTE and import them using the 'import-paths' RPC on
LOCAL.  When RECURSIVE? is true, retrieve the closure of FILES."
  (let-values (((port count)
                (file-retrieval-port files remote
                                     #:recursive? recursive?)))
    (format #t (N_ "retrieving ~a store item from '~a'...~%"
                   "retrieving ~a store items from '~a'...~%" count)
            count (remote-store-host remote))
    (when (eof-object? (lookahead-u8 port))
      ;; The failure could be because one of the requested store items is not
      ;; valid on REMOTE, or because Guile or Guix is improperly installed.
      ;; TODO: Improve error reporting.
      (raise (condition
              (&message
               (message
                (format #f
                        (G_ "failed to retrieve store items from '~a'")
                        (remote-store-host remote)))))))

    (let ((result (import-paths local port)))
      (close-port port)
      result)))

;;; ssh.scm ends here
