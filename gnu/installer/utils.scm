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
  #:use-module (gnu services herd)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 control)
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
            run-external-command-with-handler
            run-external-command-with-line-hooks
            run-command
            run-command-in-installer

            syslog-port
            %syslog-line-hook
            installer-log-port
            %installer-log-line-hook
            %default-installer-line-hooks
            installer-log-line
            call-with-time
            let/time

            with-server-socket
            current-server-socket
            current-clients
            send-to-clients

            with-silent-shepherd))

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

(define* (run-external-command-with-handler handler command)
  "Run command specified by the list COMMAND in a child with output handler
HANDLER.  HANDLER is a procedure taking an input port, to which the command
will write its standard output and error.  Returns the integer status value of
the child process as returned by waitpid."
  (match-let (((input . output) (pipe)))
    ;; Hack to work around Guile bug 52835
    (define dup-output (duplicate-port output "w"))
    ;; Void pipe, but holds the pid for close-pipe.
    (define dummy-pipe
      (with-input-from-file "/dev/null"
        (lambda ()
          (with-output-to-port output
            (lambda ()
              (with-error-to-port dup-output
                (lambda ()
                  (apply open-pipe* (cons "" command)))))))))
    (close-port output)
    (close-port dup-output)
    (handler input)
    (close-port input)
    (close-pipe dummy-pipe)))

(define (run-external-command-with-line-hooks line-hooks command)
  "Run command specified by the list COMMAND in a child, processing each
output line with the procedures in LINE-HOOKS.  Returns the integer status
value of the child process as returned by waitpid."
  (define (handler input)
    (and
     (and=> (get-line input)
            (lambda (line)
              (if (eof-object? line)
                  #f
                  (begin (for-each (lambda (f) (f line))
                                   (append line-hooks
                                       %default-installer-line-hooks))
                         #t))))
     (handler input)))
  (run-external-command-with-handler handler command))

(define* (run-command command)
  "Run COMMAND, a list of strings.  Return true if COMMAND exited
successfully, #f otherwise."
  (define (pause)
    (format #t (G_ "Press Enter to continue.~%"))
    (send-to-clients '(pause))
    (match (select (cons (current-input-port) (current-clients))
             '() '())
      (((port _ ...) _ _)
       (read-line port))))

  (installer-log-line "running command ~s" command)
  (define result (run-external-command-with-line-hooks
                  (list %display-line-hook)
                  command))
  (define exit-val (status:exit-val result))
  (define term-sig (status:term-sig result))
  (define stop-sig (status:stop-sig result))
  (define succeeded?
    (cond
     ((and exit-val (not (zero? exit-val)))
      (installer-log-line "command ~s exited with value ~a"
                          command exit-val)
      (format #t (G_ "Command ~s exited with value ~a")
              command exit-val)
      #f)
     (term-sig
      (installer-log-line "command ~s killed by signal ~a"
                          command term-sig)
      (format #t (G_ "Command ~s killed by signal ~a")
              command term-sig)
      #f)
     (stop-sig
      (installer-log-line "command ~s stopped by signal ~a"
                          command stop-sig)
      (format #t (G_ "Command ~s stopped by signal ~a")
              command stop-sig)
      #f)
     (else
      (installer-log-line "command ~s succeeded" command)
      (format #t (G_ "Command ~s succeeded") command)
      #t)))
  (newline)
  (pause)
  succeeded?)

(define run-command-in-installer
  (make-parameter
   (lambda (. args)
     (raise
      (condition
       (&serious)
       (&message (message "run-command-in-installer not set")))))))


;;;
;;; Logging.
;;;

(define (call-with-time thunk kont)
  "Call THUNK and pass KONT the elapsed time followed by THUNK's return
values."
  (let* ((start  (current-time time-monotonic))
         (result (call-with-values thunk list))
         (end    (current-time time-monotonic)))
    (apply kont (time-difference end start) result)))

(define-syntax-rule (let/time ((time result exp)) body ...)
  (call-with-time (lambda () exp) (lambda (time result) body ...)))

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

(define (%syslog-line-hook line)
  (format (syslog-port) "installer[~d]: ~a~%" (getpid) line))

(define-syntax syslog
  (lambda (s)
    "Like 'format', but write to syslog."
    (syntax-case s ()
      ((_ fmt args ...)
       (string? (syntax->datum #'fmt))
       (with-syntax ((fmt (string-append "installer[~d]: "
                                         (syntax->datum #'fmt))))
         #'(format (syslog-port) fmt (getpid) args ...))))))

(define (open-new-log-port)
  (define now (localtime (time-second (current-time))))
  (define filename
    (format #f "/tmp/installer.~a.log"
            (strftime "%F.%T" now)))
  (open filename (logior O_RDWR
                         O_CREAT)))

(define installer-log-port
  (let ((port #f))
    (lambda ()
      "Return an input and output port to the installer log."
      (unless port
        (set! port (open-new-log-port)))
      port)))

(define (%installer-log-line-hook line)
  (format (installer-log-port) "~a~%" line))

(define (%display-line-hook line)
  (display line)
  (newline))

(define %default-installer-line-hooks
  (list %syslog-line-hook
        %installer-log-line-hook))

(define-syntax installer-log-line
  (lambda (s)
    "Like 'format', but uses the default line hooks, and only formats one line."
    (syntax-case s ()
      ((_ fmt args ...)
       (string? (syntax->datum #'fmt))
       #'(let ((formatted (format #f fmt args ...)))
               (for-each (lambda (f) (f formatted))
                         %default-installer-line-hooks))))))


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
                        (installer-log-line
                         "removing client ~s due to ~s while replying"
                         (fileno client) (strerror errno))
                        (false-if-exception (close-port client))
                        remainder)
                      (cons client remainder))))))
          '()
          (current-clients)))

  (current-clients (reverse remainder))
  exp)

(define-syntax-rule (with-silent-shepherd exp ...)
  "Evaluate EXP while discarding shepherd messages."
  (parameterize ((shepherd-message-port
                  (%make-void-port "w")))
    exp ...))
