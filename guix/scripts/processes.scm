;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts processes)
  #:use-module ((guix store) #:select (%store-prefix))
  #:use-module (guix scripts)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:export (process?
            process-id
            process-parent-id
            process-command
            processes

            daemon-session?
            daemon-session-process
            daemon-session-client
            daemon-session-children
            daemon-session-locks-held
            daemon-sessions

            guix-processes))

;; Process as can be found in /proc on GNU/Linux.
(define-record-type <process>
  (process id parent command)
  process?
  (id       process-id)                           ;integer
  (parent   process-parent-id)                    ;integer | #f
  (command  process-command))                     ;list of strings

(define (write-process process port)
  (format port "#<process ~a>" (process-id process)))

(set-record-type-printer! <process> write-process)

(define (read-status-ppid port)
  "Read the PPID from PORT, an input port on a /proc/PID/status file.  Return
#f for PID 1 and kernel pseudo-processes."
  (let loop ()
    (match (read-line port)
      ((? eof-object?) #f)
      (line
       (if (string-prefix? "PPid:" line)
           (string->number (string-trim-both (string-drop line 5)))
           (loop))))))

(define %not-nul
  (char-set-complement (char-set #\nul)))

(define (read-command-line port)
  "Read the zero-split command line from PORT, a /proc/PID/cmdline file, and
return it as a list."
  (string-tokenize (read-string port) %not-nul))

(define (processes)
  "Return a list of process records representing the currently alive
processes."
  ;; This assumes a Linux-compatible /proc file system.  There exists one for
  ;; GNU/Hurd.
  (filter-map (lambda (pid)
                ;; There's a TOCTTOU race here.  If we get ENOENT, simply
                ;; ignore PID.
                (catch 'system-error
                  (lambda ()
                    (define ppid
                      (call-with-input-file (string-append "/proc/" pid "/status")
                        read-status-ppid))
                    (define command
                      (call-with-input-file (string-append "/proc/" pid "/cmdline")
                        read-command-line))
                    (process (string->number pid) ppid command))
                  (lambda args
                    (if (= ENOENT (system-error-errno args))
                        #f
                        (apply throw args)))))
              (scandir "/proc" string->number)))

(define (process-open-files process)
  "Return the list of files currently open by PROCESS."
  (let ((directory (string-append "/proc/"
                                  (number->string (process-id process))
                                  "/fd")))
    (filter-map (lambda (fd)
                  ;; There's a TOCTTOU race here, hence the 'catch'.
                  (catch 'system-error
                    (lambda ()
                      (readlink (string-append directory "/" fd)))
                    (lambda args
                      (if (= ENOENT (system-error-errno args))
                          #f
                          (apply throw args)))))
                (or (scandir directory string->number) '()))))

;; Daemon session.
(define-record-type <daemon-session>
  (daemon-session process client children locks)
  daemon-session?
  (process    daemon-session-process)             ;<process>
  (client     daemon-session-client)              ;<process>
  (children   daemon-session-children)            ;list of <process>
  (locks      daemon-session-locks-held))         ;list of strings

(define (daemon-sessions)
  "Return two values: the list of <daemon-session> denoting the currently
active sessions, and the master 'guix-daemon' process."
  (define (lock-file? file)
    (and (string-prefix? (%store-prefix) file)
         (string-suffix? ".lock" file)))

  (let* ((processes (processes))
         (daemons   (filter (lambda (process)
                              (match (process-command process)
                                ((argv0 _ ...)
                                 (string=? (basename argv0) "guix-daemon"))
                                (_ #f)))
                            processes))
         (children  (filter (lambda (process)
                              (match (process-command process)
                                ((argv0 (= string->number argv1) _ ...)
                                 (integer? argv1))
                                (_ #f)))
                            daemons))
         (master    (remove (lambda (process)
                              (memq process children))
                            daemons)))
    (define (lookup-process pid)
      (find (lambda (process)
              (and (process-id process)
                   (= pid (process-id process))))
            processes))

    (define (lookup-children pid)
      (filter (lambda (process)
                (and (process-parent-id process)
                     (= pid (process-parent-id process))))
              processes))

    (values (map (lambda (process)
                   (match (process-command process)
                     ((argv0 (= string->number client) _ ...)
                      (let ((files (process-open-files process)))
                        (daemon-session process
                                        (lookup-process client)
                                        (lookup-children (process-id process))
                                        (filter lock-file? files))))))
                 children)
            master)))

(define (daemon-session->recutils session port)
  "Display SESSION information in recutils format on PORT."
  (format port "SessionPID: ~a~%"
          (process-id (daemon-session-process session)))
  (format port "ClientPID: ~a~%"
          (process-id (daemon-session-client session)))
  (format port "ClientCommand:~{ ~a~}~%"
          (process-command (daemon-session-client session)))
  (for-each (lambda (lock)
              (format port "LockHeld: ~a~%" lock))
            (daemon-session-locks-held session))
  (for-each (lambda (process)
              (format port "ChildProcess: ~a:~{ ~a~}~%"
                      (process-id process)
                      (process-command process)))
            (daemon-session-children session)))


;;;
;;; Options.
;;;

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix processes")))))

(define (show-help)
  (display (G_ "Usage: guix processes
List the current Guix sessions and their processes."))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry point.
;;;

(define (guix-processes . args)
  (define options
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (G_ "~A: unrecognized option~%") name))
                cons
                '()))

  (for-each (lambda (session)
              (daemon-session->recutils session (current-output-port))
              (newline))
            (daemon-sessions)))
