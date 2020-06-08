;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu installer utils)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:export (read-lines
            read-all
            nearest-exact-integer
            read-percentage
            run-command

            syslog-port
            syslog

            with-server-socket
            current-server-socket
            current-clients
            send-to-clients))

(define* (read-lines #:optional (port (current-input-port)))
  "Read lines from PORT and return them as a list."
  (let loop ((line (read-line port))
             (lines '()))
    (if (eof-object? line)
        (reverse lines)
        (loop (read-line port)
              (cons line lines)))))

(define (read-all file)
  "Return the content of the given FILE as a string."
  (call-with-input-file file
    get-string-all))

(define (nearest-exact-integer x)
  "Given a real number X, return the nearest exact integer, with ties going to
the nearest exact even integer."
  (inexact->exact (round x)))

(define (read-percentage percentage)
  "Read PERCENTAGE string and return the corresponding percentage as a
number. If no percentage is found, return #f"
  (let ((result (string-match "^([0-9]+)%$" percentage)))
    (and result
         (string->number (match:substring result 1)))))


;;;
;;; Logging.
;;;

(define (open-syslog-port)
  "Return an open port (a socket) to /dev/log or #f if that wasn't possible."
  (let ((sock (socket AF_UNIX SOCK_DGRAM 0)))
    (catch 'system-error
      (lambda ()
        (connect sock AF_UNIX "/dev/log")
        (setvbuf sock 'line)
        sock)
      (lambda args
        (close-port sock)
        #f))))

(define syslog-port
  (let ((port #f))
    (lambda ()
      "Return an output port to syslog."
      (unless port
        (set! port (open-syslog-port)))
      (or port (%make-void-port "w")))))

(define-syntax syslog
  (lambda (s)
    "Like 'format', but write to syslog."
    (syntax-case s ()
      ((_ fmt args ...)
       (string? (syntax->datum #'fmt))
       (with-syntax ((fmt (string-append "installer[~d]: "
                                         (syntax->datum #'fmt))))
         #'(format (syslog-port) fmt (getpid) args ...))))))


;;;
;;; Client protocol.
;;;

(define %client-socket-file
  ;; Unix-domain socket where the installer accepts connections.
  "/var/guix/installer-socket")

(define current-server-socket
  ;; Socket on which the installer is currently accepting connections, or #f.
  (make-parameter #f))

(define current-clients
  ;; List of currently connected clients.
  (make-parameter '()))

(define* (open-server-socket
          #:optional (socket-file %client-socket-file))
  "Open SOCKET-FILE as a Unix-domain socket to accept incoming connections and
return it."
  (mkdir-p (dirname socket-file))
  (when (file-exists? socket-file)
    (delete-file socket-file))
  (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
    (bind sock AF_UNIX socket-file)
    (listen sock 0)
    sock))

(define (call-with-server-socket thunk)
  (if (current-server-socket)
      (thunk)
      (let ((socket (open-server-socket)))
        (dynamic-wind
          (const #t)
          (lambda ()
            (parameterize ((current-server-socket socket))
              (thunk)))
          (lambda ()
            (close-port socket))))))

(define-syntax-rule (with-server-socket exp ...)
  "Evaluate EXP with 'current-server-socket' parameterized to a currently
accepting socket."
  (call-with-server-socket (lambda () exp ...)))

(define* (send-to-clients exp)
  "Send EXP to all the current clients."
  (define remainder
    (fold (lambda (client remainder)
            (catch 'system-error
              (lambda ()
                (write exp client)
                (newline client)
                (force-output client)
                (cons client remainder))
              (lambda args
                ;; We might get EPIPE if the client disconnects; when that
                ;; happens, remove CLIENT from the set of available clients.
                (let ((errno (system-error-errno args)))
                  (if (memv errno (list EPIPE ECONNRESET ECONNABORTED))
                      (begin
                        (syslog "removing client ~s due to ~s while replying~%"
                                (fileno client) (strerror errno))
                        (false-if-exception (close-port client))
                        remainder)
                      (cons client remainder))))))
          '()
          (current-clients)))

  (current-clients (reverse remainder))
  exp)


;;;
;;; Run commands.
;;;

;; XXX: This is taken from (guix build utils) and could be factorized.
(define (open-pipe-with-stderr program . args)
  "Run PROGRAM with ARGS in an input pipe, but, unlike 'open-pipe*', redirect
both its standard output and standard error to the pipe.  Return two value:
the pipe to read PROGRAM's data from, and the PID of the child process running
PROGRAM."
  ;; 'open-pipe*' doesn't attempt to capture stderr in any way, which is why
  ;; we need to roll our own.
  (match (pipe)
    ((input .  output)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #t)
          (lambda ()
            (close-port input)
            (close-port (syslog-port))
            (dup2 (fileno output) 1)
            (dup2 (fileno output) 2)
            (apply execlp program program args))
          (lambda ()
            (primitive-exit 127))))
       (pid
        (close-port output)
        (values input pid))))))

(define invoke-log-port
  ;; Port used by INVOKE-WITH-LOG for logging.
  (make-parameter #f))

(define* (invoke-with-log program . args)
  "Invoke PROGRAM with ARGS and log PROGRAM's standard output and standard
error to INVOKE-LOG-PORT.  If PROGRAM succeeds, print nothing and return the
unspecified value; otherwise, raise a '&message' error condition with the
status code.  This procedure is very similar to INVOKE/QUIET with the
noticeable difference that the program output, that can be quite heavy, is not
stored but directly sent to INVOKE-LOG-PORT if defined."
  (let-values (((pipe pid)
                (apply open-pipe-with-stderr program args)))
    (let loop ()
      (match (read-line pipe)
        ((? eof-object?)
         (close-port pipe)
         (match (waitpid pid)
           ((_ . status)
            (unless (zero? status)
              (raise
               (condition (&invoke-error
                           (program program)
                           (arguments args)
                           (exit-status (status:exit-val status))
                           (term-signal (status:term-sig status))
                           (stop-signal (status:stop-sig status)))))))))
        (line
         (and=> (invoke-log-port) (cut format <> "~a~%" line))
         (loop))))))

(define* (run-command command #:key locale)
  "Run COMMAND, a list of strings, in the given LOCALE.  Return true if
COMMAND exited successfully, #f otherwise."
  (define env (environ))

  (define (pause)
    (format #t (G_ "Press Enter to continue.~%"))
    (send-to-clients '(pause))
    (environ env)                               ;restore environment variables
    (match (select (cons (current-input-port) (current-clients))
             '() '())
      (((port _ ...) _ _)
       (read-line port))))

  (setenv "PATH" "/run/current-system/profile/bin")

  (when locale
    (let ((supported? (false-if-exception
                       (setlocale LC_ALL locale))))
      ;; If LOCALE is not supported, then set LANGUAGE, which might at
      ;; least give us translated messages.
      (if supported?
          (setenv "LC_ALL" locale)
          (setenv "LANGUAGE"
                  (string-take locale
                               (or (string-index locale #\_)
                                   (string-length locale)))))))

  (guard (c ((invoke-error? c)
             (newline)
             (format (current-error-port)
                     (G_ "Command failed with exit code ~a.~%")
                     (invoke-error-exit-status c))
             (syslog "command ~s failed with exit code ~a"
                     command (invoke-error-exit-status c))
             (pause)
             #f))
    (syslog "running command ~s~%" command)
    ;; If there are any connected clients, assume that we are running
    ;; installation tests. In that case, dump the standard and error outputs
    ;; to syslog.
    (let ((testing? (not (null? (current-clients)))))
      (if testing?
          (parameterize ((invoke-log-port (syslog-port)))
            (apply invoke-with-log command))
          (apply invoke command)))
    (syslog "command ~s succeeded~%" command)
    (newline)
    (pause)
    #t))

;;; utils.scm ends here
