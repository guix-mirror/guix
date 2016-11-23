;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build marionette)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:export (marionette?
            make-marionette
            marionette-eval
            marionette-control
            %qwerty-us-keystrokes
            marionette-type))

;;; Commentary:
;;;
;;; Instrumentation tools for QEMU virtual machines (VMs).  A "marionette" is
;;; essentially a VM (a QEMU instance) with its monitor connected to a
;;; Unix-domain socket, and with a REPL inside the guest listening on a
;;; virtual console, which is itself connected to the host via a Unix-domain
;;; socket--these are the marionette's strings, connecting it to the almighty
;;; puppeteer.
;;;
;;; Code:

(define-record-type <marionette>
  (marionette command pid monitor repl)
  marionette?
  (command    marionette-command)                 ;list of strings
  (pid        marionette-pid)                     ;integer
  (monitor    marionette-monitor)                 ;port
  (repl       marionette-repl))                   ;port

(define* (wait-for-monitor-prompt port #:key (quiet? #t))
  "Read from PORT until we have seen all of QEMU's monitor prompt.  When
QUIET? is false, the monitor's output is written to the current output port."
  (define full-prompt
    (string->list "(qemu) "))

  (let loop ((prompt full-prompt)
             (matches '())
             (prefix  '()))
    (match prompt
      (()
       ;; It's useful to set QUIET? so we don't display the echo of our own
       ;; commands.
       (unless quiet?
         (for-each (lambda (line)
                     (format #t "qemu monitor: ~a~%" line))
                   (string-tokenize (list->string (reverse prefix))
                                    (char-set-complement (char-set #\newline))))))
      ((chr rest ...)
       (let ((read (read-char port)))
         (cond ((eqv? read chr)
                (loop rest (cons read matches) prefix))
               ((eof-object? read)
                (error "EOF while waiting for QEMU monitor prompt"
                       (list->string (reverse prefix))))
               (else
                (loop full-prompt
                      '()
                      (cons read (append matches prefix))))))))))

(define* (make-marionette command
                          #:key (socket-directory "/tmp") (timeout 20))
  "Return a QEMU marionette--i.e., a virtual machine with open connections to the
QEMU monitor and to the guest's backdoor REPL."
  (define (file->sockaddr file)
    (make-socket-address AF_UNIX
                         (string-append socket-directory "/" file)))

  (define extra-options
    (list "-nographic"
          "-monitor" (string-append "unix:" socket-directory "/monitor")
          "-chardev" (string-append "socket,id=repl,path=" socket-directory
                                    "/repl")
          "-device" "virtio-serial"
          "-device" "virtconsole,chardev=repl"))

  (define (accept* port)
    (match (select (list port) '() (list port) timeout)
      (((port) () ())
       (accept port))
      (_
       (error "timeout in 'accept'" port))))

  (let ((monitor (socket AF_UNIX SOCK_STREAM 0))
        (repl    (socket AF_UNIX SOCK_STREAM 0)))
    (bind monitor (file->sockaddr "monitor"))
    (listen monitor 1)
    (bind repl (file->sockaddr "repl"))
    (listen repl 1)

    (match (primitive-fork)
      (0
       (catch #t
         (lambda ()
           (close monitor)
           (close repl)
           (match command
             ((program . args)
              (apply execl program program
                     (append args extra-options)))))
         (lambda (key . args)
           (print-exception (current-error-port)
                            (stack-ref (make-stack #t) 1)
                            key args)
           (primitive-exit 1))))
      (pid
       (format #t "QEMU runs as PID ~a~%" pid)

       (match (accept* monitor)
         ((monitor-conn . _)
          (display "connected to QEMU's monitor\n")
          (close-port monitor)
          (wait-for-monitor-prompt monitor-conn)
          (display "read QEMU monitor prompt\n")
          (match (accept* repl)
            ((repl-conn . addr)
             (display "connected to guest REPL\n")
             (close-port repl)
             (match (read repl-conn)
               ('ready
                (alarm 0)
                (display "marionette is ready\n")
                (marionette (append command extra-options) pid
                            monitor-conn repl-conn)))))))))))

(define (marionette-eval exp marionette)
  "Evaluate EXP in MARIONETTE's backdoor REPL.  Return the result."
  (match marionette
    (($ <marionette> command pid monitor repl)
     (write exp repl)
     (newline repl)
     (read repl))))

(define (marionette-control command marionette)
  "Run COMMAND in the QEMU monitor of MARIONETTE.  COMMAND is a string such as
\"sendkey ctrl-alt-f1\" or \"screendump foo.ppm\" (info \"(qemu-doc)
pcsys_monitor\")."
  (match marionette
    (($ <marionette> _ _ monitor)
     (display command monitor)
     (newline monitor)
     (wait-for-monitor-prompt monitor))))

(define %qwerty-us-keystrokes
  ;; Maps "special" characters to their keystrokes.
  '((#\newline . "ret")
    (#\space . "spc")
    (#\- . "minus")
    (#\+ . "shift-equal")
    (#\* . "shift-8")
    (#\= . "equal")
    (#\? . "shift-slash")
    (#\[ . "bracket_left")
    (#\] . "bracket_right")
    (#\( . "shift-9")
    (#\) . "shift-0")
    (#\/ . "slash")
    (#\< . "less")
    (#\> . "shift-less")
    (#\. . "dot")
    (#\, . "comma")
    (#\; . "semicolon")
    (#\bs . "backspace")
    (#\tab . "tab")))

(define* (string->keystroke-commands str
                                     #:optional
                                     (keystrokes
                                      %qwerty-us-keystrokes))
  "Return a list of QEMU monitor commands to send the keystrokes corresponding
to STR.  KEYSTROKES is an alist specifying a mapping from characters to
keystrokes."
  (string-fold-right (lambda (chr result)
                       (cons (string-append "sendkey "
                                            (or (assoc-ref keystrokes chr)
                                                (string chr)))
                             result))
                     '()
                     str))

(define* (marionette-type str marionette
                          #:key (keystrokes %qwerty-us-keystrokes))
  "Type STR on MARIONETTE's keyboard, using the KEYSTROKES alist to map characters
to actual keystrokes."
  (for-each (cut marionette-control <> marionette)
            (string->keystroke-commands str keystrokes)))

;;; marionette.scm ends here
