;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix progress)
  #:use-module (guix records)
  #:use-module (srfi srfi-19)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (<progress-reporter>
            progress-reporter
            make-progress-reporter
            progress-reporter?
            call-with-progress-reporter

            start-progress-reporter!
            stop-progress-reporter!
            progress-reporter-report!

            progress-reporter/silent
            progress-reporter/file
            progress-reporter/bar

            byte-count->string
            current-terminal-columns

            dump-port*))

;;; Commentary:
;;;
;;; Helper to write progress report code for downloads, etc.
;;;
;;; Code:

(define-record-type* <progress-reporter>
  progress-reporter make-progress-reporter progress-reporter?
  (start   progress-reporter-start)     ; thunk
  (report  progress-reporter-report)    ; procedure
  (stop    progress-reporter-stop))     ; thunk

(define (call-with-progress-reporter reporter proc)
  "Start REPORTER for progress reporting, and call @code{(@var{proc} report)}
with the resulting report procedure.  When @var{proc} returns, the REPORTER is
stopped."
  (match reporter
    (($ <progress-reporter> start report stop)
     (dynamic-wind start (lambda () (proc report)) stop))))

(define (start-progress-reporter! reporter)
  "Low-level procedure to start REPORTER."
  (match reporter
    (($ <progress-reporter> start report stop)
     (start))))

(define (progress-reporter-report! reporter)
  "Low-level procedure to lead REPORTER to emit a report."
  (match reporter
    (($ <progress-reporter> start report stop)
     (report))))

(define (stop-progress-reporter! reporter)
  "Low-level procedure to stop REPORTER."
  (match reporter
    (($ <progress-reporter> start report stop)
     (stop))))

(define progress-reporter/silent
  (make-progress-reporter noop noop noop))


;;;
;;; File download progress report.
;;;

(cond-expand
  (guile-2.2
   ;; Guile 2.2.2 has a bug whereby 'time-monotonic' objects have seconds and
   ;; nanoseconds swapped (fixed in Guile commit 886ac3e).  Work around it.
   (define time-monotonic time-tai))
  (else #t))

(define (nearest-exact-integer x)
  "Given a real number X, return the nearest exact integer, with ties going to
the nearest exact even integer."
  (inexact->exact (round x)))

(define (duration->seconds duration)
  "Return the number of seconds represented by DURATION, a 'time-duration'
object, as an inexact number."
  (+ (time-second duration)
     (/ (time-nanosecond duration) 1e9)))

(define (seconds->string duration)
  "Given DURATION in seconds, return a string representing it in 'mm:ss' or
'hh:mm:ss' format, as needed."
  (if (not (number? duration))
      "00:00"
      (let* ((total-seconds (nearest-exact-integer duration))
             (extra-seconds (modulo total-seconds 3600))
             (num-hours     (quotient total-seconds 3600))
             (hours         (and (positive? num-hours) num-hours))
             (mins          (quotient extra-seconds 60))
             (secs          (modulo extra-seconds 60)))
        (format #f "~@[~2,'0d:~]~2,'0d:~2,'0d" hours mins secs))))

(define (byte-count->string size)
  "Given SIZE in bytes, return a string representing it in a human-readable
way."
  (let ((KiB 1024.)
        (MiB (expt 1024. 2))
        (GiB (expt 1024. 3))
        (TiB (expt 1024. 4)))
    (cond
     ((< size KiB) (format #f "~dB"     (nearest-exact-integer size)))
     ((< size MiB) (format #f "~dKiB"   (nearest-exact-integer (/ size KiB))))
     ((< size GiB) (format #f "~,1fMiB" (/ size MiB)))
     ((< size TiB) (format #f "~,2fGiB" (/ size GiB)))
     (else         (format #f "~,3fTiB" (/ size TiB))))))

(define (string-pad-middle left right len)
  "Combine LEFT and RIGHT with enough padding in the middle so that the
resulting string has length at least LEN (it may overflow).  If the string
does not overflow, the last char in RIGHT will be flush with the LEN
column."
  (let* ((total-used (+ (string-length left)
                        (string-length right)))
         (num-spaces (max 1 (- len total-used)))
         (padding    (make-string num-spaces #\space)))
    (string-append left padding right)))

(define (rate-limited proc interval)
  "Return a procedure that will forward the invocation to PROC when the time
elapsed since the previous forwarded invocation is greater or equal to
INTERVAL (a time-duration object), otherwise does nothing and returns #f."
  (let ((previous-at #f))
    (lambda args
      (let* ((now (current-time time-monotonic))
             (forward-invocation (lambda ()
                                   (set! previous-at now)
                                   (apply proc args))))
        (if previous-at
            (let ((elapsed (time-difference now previous-at)))
              (if (time>=? elapsed interval)
                  (forward-invocation)
                  #f))
            (forward-invocation))))))

(define current-terminal-columns
  ;; Number of columns of the terminal.
  (make-parameter 80))

(define* (progress-bar % #:optional (bar-width 20))
  "Return % as a string representing an ASCII-art progress bar.  The total
width of the bar is BAR-WIDTH."
  (let* ((bar-width (max 3 (- bar-width 2)))
         (fraction (/ % 100))
         (filled   (inexact->exact (floor (* fraction bar-width))))
         (empty    (- bar-width filled)))
    (format #f "[~a~a]"
            (make-string filled #\#)
            (make-string empty #\space))))

(define (erase-current-line port)
  "Write an ANSI erase-current-line sequence to PORT to erase the whole line and
move the cursor to the beginning of the line."
  (display "\r\x1b[K" port))

(define* (progress-reporter/file file size
                                 #:optional (log-port (current-output-port))
                                 #:key (abbreviation basename))
  "Return a <progress-reporter> object to show the progress of FILE's download,
which is SIZE bytes long.  The progress report is written to LOG-PORT, with
ABBREVIATION used to shorten FILE for display."
  (let ((start-time (current-time time-monotonic))
        (transferred 0))
    (define (render)
      "Write the progress report to LOG-PORT."
      (define elapsed
        (duration->seconds
         (time-difference (current-time time-monotonic) start-time)))
      (if (number? size)
          (let* ((%  (* 100.0 (/ transferred size)))
                 (throughput (/ transferred elapsed))
                 (left       (format #f " ~a  ~a"
                                     (abbreviation file)
                                     (byte-count->string size)))
                 (right      (format #f "~a/s ~a ~a~6,1f%"
                                     (byte-count->string throughput)
                                     (seconds->string elapsed)
                                     (progress-bar %) %)))
            (erase-current-line log-port)
            (display (string-pad-middle left right
                                        (current-terminal-columns))
                     log-port)
            (force-output log-port))
          (let* ((throughput (/ transferred elapsed))
                 (left       (format #f " ~a"
                                     (abbreviation file)))
                 (right      (format #f "~a/s ~a | ~a transferred"
                                     (byte-count->string throughput)
                                     (seconds->string elapsed)
                                     (byte-count->string transferred))))
            (erase-current-line log-port)
            (display (string-pad-middle left right
                                        (current-terminal-columns))
                     log-port)
            (force-output log-port))))

    (progress-reporter
     (start render)
     ;; Report the progress every 300ms or longer.
     (report
      (let ((rate-limited-render
             (rate-limited render (make-time time-monotonic 300000000 0))))
        (lambda (value)
          (set! transferred value)
          (rate-limited-render))))
     ;; Don't miss the last report.
     (stop render))))

(define* (progress-reporter/bar total
                                #:optional
                                (prefix "")
                                (port (current-error-port)))
  "Return a reporter that shows a progress bar every time one of the TOTAL
tasks is performed.  Write PREFIX at the beginning of the line."
  (define done 0)

  (define (report-progress)
    (set! done (+ 1 done))
    (unless (> done total)
      (let* ((ratio (* 100. (/ done total))))
        (erase-current-line port)
        (if (string-null? prefix)
            (display (progress-bar ratio (current-terminal-columns)) port)
            (let ((width (- (current-terminal-columns)
                            (string-length prefix) 3)))
              (display prefix port)
              (display "  " port)
              (display (progress-bar ratio width) port)))
        (force-output port))))

  (progress-reporter
   (start (lambda ()
            (set! done 0)))
   (report report-progress)
   (stop (lambda ()
           (erase-current-line port)
           (unless (string-null? prefix)
             (display prefix port)
             (newline port))
           (force-output port)))))

;; TODO: replace '(@ (guix build utils) dump-port))'.
(define* (dump-port* in out
                     #:key (buffer-size 16384)
                     (reporter progress-reporter/silent))
  "Read as much data as possible from IN and write it to OUT, using chunks of
BUFFER-SIZE bytes.  After each successful transfer of BUFFER-SIZE bytes or
less, report the total number of bytes transferred to the REPORTER, which
should be a <progress-reporter> object."
  (define buffer
    (make-bytevector buffer-size))

  (call-with-progress-reporter reporter
    (lambda (report)
      (let loop ((total 0)
                 (bytes (get-bytevector-n! in buffer 0 buffer-size)))
        (or (eof-object? bytes)
            (let ((total (+ total bytes)))
              (put-bytevector out buffer 0 bytes)
              (report total)
              (loop total (get-bytevector-n! in buffer 0 buffer-size))))))))
