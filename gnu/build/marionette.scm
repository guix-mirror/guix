;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
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
  #:use-module (ice-9 popen)
  #:export (marionette?
            make-marionette
            marionette-eval
            wait-for-file
            wait-for-tcp-port
            wait-for-unix-socket
            marionette-control
            marionette-screen-text
            wait-for-screen-text
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
  (repl       %marionette-repl))                  ;promise of a port

(define-syntax-rule (marionette-repl marionette)
  (force (%marionette-repl marionette)))

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

          ;; See
          ;; <http://www.linux-kvm.org/page/VMchannel_Requirements#Invocation>.
          "-device" "virtio-serial"
          "-device" "virtserialport,chardev=repl,name=org.gnu.guix.port.0"))

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

          (marionette (append command extra-options) pid
                      monitor-conn

                      ;; The following 'accept' call connects immediately, but
                      ;; we don't know whether the guest has connected until
                      ;; we actually receive the 'ready' message.
                      (match (accept* repl)
                        ((repl-conn . addr)
                         (display "connected to guest REPL\n")
                         (close-port repl)
                         ;; Delay reception of the 'ready' message so that the
                         ;; caller can already send monitor commands.
                         (delay
                           (match (read repl-conn)
                             ('ready
                              (display "marionette is ready\n")
                              repl-conn))))))))))))

(define (marionette-eval exp marionette)
  "Evaluate EXP in MARIONETTE's backdoor REPL.  Return the result."
  (match marionette
    (($ <marionette> command pid monitor (= force repl))
     (write exp repl)
     (newline repl)
     (read repl))))

(define* (wait-for-file file marionette
                        #:key (timeout 10) (read 'read))
  "Wait until FILE exists in MARIONETTE; READ its content and return it.  If
FILE has not shown up after TIMEOUT seconds, raise an error."
  (match (marionette-eval
          `(let loop ((i ,timeout))
             (cond ((file-exists? ,file)
                    (cons 'success (call-with-input-file ,file ,read)))
                   ((> i 0)
                    (sleep 1)
                    (loop (- i 1)))
                   (else
                    'failure)))
          marionette)
    (('success . result)
     result)
    ('failure
     (error "file didn't show up" file))))

(define* (wait-for-tcp-port port marionette
                            #:key (timeout 20))
  "Wait for up to TIMEOUT seconds for PORT to accept connections in
MARIONETTE.  Raise an error on failure."
  ;; Note: The 'connect' loop has to run within the guest because, when we
  ;; forward ports to the host, connecting to the host never raises
  ;; ECONNREFUSED.
  (match (marionette-eval
          `(begin
             (let ((sock (socket PF_INET SOCK_STREAM 0)))
               (let loop ((i 0))
                 (catch 'system-error
                   (lambda ()
                     (connect sock AF_INET INADDR_LOOPBACK ,port)
                     'success)
                   (lambda args
                     (if (< i ,timeout)
                         (begin
                           (sleep 1)
                           (loop (+ 1 i)))
                         'failure))))))
          marionette)
    ('success #t)
    ('failure
     (error "nobody's listening on port" port))))

(define* (wait-for-unix-socket file-name marionette
                                #:key (timeout 20))
  "Wait for up to TIMEOUT seconds for FILE-NAME, a Unix domain socket, to
accept connections in MARIONETTE.  Raise an error on failure."
  (match (marionette-eval
          `(begin
             (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
               (let loop ((i 0))
                 (catch 'system-error
                   (lambda ()
                     (connect sock AF_UNIX ,file-name)
                     'success)
                   (lambda args
                     (if (< i ,timeout)
                         (begin
                           (sleep 1)
                           (loop (+ 1 i)))
                         'failure))))))
          marionette)
    ('success #t)
    ('failure
     (error "nobody's listening on unix domain socket" file-name))))

(define (marionette-control command marionette)
  "Run COMMAND in the QEMU monitor of MARIONETTE.  COMMAND is a string such as
\"sendkey ctrl-alt-f1\" or \"screendump foo.ppm\" (info \"(qemu-doc)
pcsys_monitor\")."
  (match marionette
    (($ <marionette> _ _ monitor)
     (display command monitor)
     (newline monitor)
     ;; The "quit" command terminates QEMU immediately, with no output.
     (unless (string=? command "quit") (wait-for-monitor-prompt monitor)))))

(define* (marionette-screen-text marionette
                                 #:key
                                 (ocrad "ocrad"))
  "Take a screenshot of MARIONETTE, perform optical character
recognition (OCR), and return the text read from the screen as a string.  Do
this by invoking OCRAD (file name for GNU Ocrad's command)"
  (define (random-file-name)
    (string-append "/tmp/marionette-screenshot-"
                   (number->string (random (expt 2 32)) 16)
                   ".ppm"))

  (let ((image (random-file-name)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (marionette-control (string-append "screendump " image)
                            marionette)

        ;; Tell Ocrad to invert the image colors (make it black on white) and
        ;; to scale the image up, which significantly improves the quality of
        ;; the result.  In spite of this, be aware that OCR confuses "y" and
        ;; "V" and sometimes erroneously introduces white space.
        (let* ((pipe (open-pipe* OPEN_READ ocrad
                                 "-i" "-s" "10" image))
               (text (get-string-all pipe)))
          (unless (zero? (close-pipe pipe))
            (error "'ocrad' failed" ocrad))
          text))
      (lambda ()
        (false-if-exception (delete-file image))))))

(define* (wait-for-screen-text marionette predicate
                               #:key (timeout 30) (ocrad "ocrad"))
  "Wait for TIMEOUT seconds or until the screen text on MARIONETTE matches
PREDICATE, whichever comes first.  Raise an error when TIMEOUT is exceeded."
  (define start
    (car (gettimeofday)))

  (define end
    (+ start timeout))

  (let loop ()
    (if (> (car (gettimeofday)) end)
        (error "'wait-for-screen-text' timeout" predicate)
        (or (predicate (marionette-screen-text marionette #:ocrad ocrad))
            (begin
              (sleep 1)
              (loop))))))

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
    (#\{ . "shift-bracket_left")
    (#\} . "shift-bracket_right")
    (#\( . "shift-9")
    (#\) . "shift-0")
    (#\/ . "slash")
    (#\< . "less")
    (#\> . "shift-less")
    (#\. . "dot")
    (#\, . "comma")
    (#\; . "semicolon")
    (#\' . "apostrophe")
    (#\" . "shift-apostrophe")
    (#\` . "grave_accent")
    (#\bs . "backspace")
    (#\tab . "tab")))

(define (character->keystroke chr keystrokes)
  "Return the keystroke for CHR according to the keyboard layout defined by
KEYSTROKES."
  (if (char-set-contains? char-set:upper-case chr)
      (string-append "shift-" (string (char-downcase chr)))
      (or (assoc-ref keystrokes chr)
          (string chr))))

(define* (string->keystroke-commands str
                                     #:optional
                                     (keystrokes
                                      %qwerty-us-keystrokes))
  "Return a list of QEMU monitor commands to send the keystrokes corresponding
to STR.  KEYSTROKES is an alist specifying a mapping from characters to
keystrokes."
  (string-fold-right (lambda (chr result)
                       (cons (string-append
                              "sendkey "
                              (character->keystroke chr keystrokes))
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
