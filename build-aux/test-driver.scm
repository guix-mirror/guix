#!/bin/sh
exec guile --no-auto-compile -e main -s "$0" "$@"
!#
;;;; test-driver.scm - Guile test driver for Automake testsuite harness

(define script-version "2021-02-02.05") ;UTC

;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:
;;;
;;; This script provides a Guile test driver using the SRFI-64 Scheme API for
;;; test suites.  SRFI-64 is distributed with Guile since version 2.0.9.
;;;
;;;; Code:

(use-modules (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 pretty-print)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (srfi srfi-64))

(define (show-help)
  (display "Usage:
   test-driver --test-name=NAME --log-file=PATH --trs-file=PATH
               [--expect-failure={yes|no}] [--color-tests={yes|no}]
               [--select=REGEXP] [--exclude=REGEXP] [--errors-only={yes|no}]
               [--enable-hard-errors={yes|no}] [--brief={yes|no}}]
               [--show-duration={yes|no}] [--]
               TEST-SCRIPT [TEST-SCRIPT-ARGUMENTS]
The '--test-name' option is mandatory.  The '--select' and '--exclude' options
allow selecting or excluding individual test cases via a regexp, respectively.
The '--errors-only' option can be set to \"yes\" to limit the logged test case
metadata to only those test cases that failed.  When set to \"yes\", the
'--brief' option disables printing the individual test case result to the
console.  When '--show-duration' is set to \"yes\", the time elapsed per test
case is shown.\n"))

(define %options
  '((test-name                 (value #t))
    (log-file                  (value #t))
    (trs-file                  (value #t))
    (select                    (value #t))
    (exclude                   (value #t))
    (errors-only               (value #t))
    (color-tests               (value #t))
    (expect-failure            (value #t)) ;XXX: not implemented yet
    (enable-hard-errors        (value #t)) ;not implemented in SRFI-64
    (brief                     (value #t))
    (show-duration             (value #t))
    (help    (single-char #\h) (value #f))
    (version (single-char #\V) (value #f))))

(define (option->boolean options key)
  "Return #t if the value associated with KEY in OPTIONS is \"yes\"."
  (and=> (option-ref options key #f) (cut string=? <> "yes")))

(define* (test-display field value  #:optional (port (current-output-port))
                       #:key pretty?)
  "Display \"FIELD: VALUE\\n\" on PORT."
  (if pretty?
      (begin
        (format port "~A:~%" field)
        (pretty-print value port #:per-line-prefix "+ "))
      (format port "~A: ~S~%" field value)))

(define* (result->string symbol #:key colorize?)
  "Return SYMBOL as an upper case string.  Use colors when COLORIZE is #t."
  (let ((result (string-upcase (symbol->string symbol))))
    (if colorize?
        (string-append (case symbol
                         ((pass)       "[0;32m")  ;green
                         ((xfail)      "[1;32m")  ;light green
                         ((skip)       "[1;34m")  ;blue
                         ((fail xpass) "[0;31m")  ;red
                         ((error)      "[0;35m")) ;magenta
                       result
                       "[m")          ;no color
        result)))


;;;
;;; SRFI 64 custom test runner.
;;;

(define* (test-runner-gnu test-name #:key color? brief? errors-only?
                          show-duration?
                          (out-port (current-output-port))
                          (trs-port (%make-void-port "w"))
                          select exclude)
  "Return an custom SRFI-64 test runner.  TEST-NAME is a string specifying the
file name of the current the test.  COLOR? specifies whether to use colors.
When BRIEF? is true, the individual test cases results are masked and only the
summary is shown.  ERRORS-ONLY? reduces the amount of test case metadata
logged to only that of the failed test cases.  OUT-PORT and TRS-PORT must be
output ports.  OUT-PORT defaults to the current output port, while TRS-PORT
defaults to a void port, which means no TRS output is logged.  SELECT and
EXCLUDE may take a regular expression to select or exclude individual test
cases based on their names."

  (define test-cases-start-time (make-hash-table))

  (define (test-on-test-begin-gnu runner)
    ;; Procedure called at the start of an individual test case, before the
    ;; test expression (and expected value) are evaluated.
    (let ((test-case-name (test-runner-test-name runner))
          (start-time     (current-time time-monotonic)))
      (hash-set! test-cases-start-time test-case-name start-time)))

  (define (test-skipped? runner)
    (eq? 'skip (test-result-kind runner)))

  (define (test-failed? runner)
    (not (or (test-passed? runner)
             (test-skipped? runner))))

  (define (test-on-test-end-gnu runner)
    ;; Procedure called at the end of an individual test case, when the result
    ;; of the test is available.
    (let* ((results (test-result-alist runner))
           (result? (cut assq <> results))
           (result  (cut assq-ref results <>))
           (test-case-name (test-runner-test-name runner))
           (start (hash-ref test-cases-start-time test-case-name))
           (end (current-time time-monotonic))
           (time-elapsed (time-difference end start))
           (time-elapsed-seconds (+ (time-second time-elapsed)
                                    (* 1e-9 (time-nanosecond time-elapsed)))))
      (unless (or brief? (and errors-only? (test-skipped? runner)))
        ;; Display the result of each test case on the console.
        (format out-port "~a: ~a - ~a ~@[[~,3fs]~]~%"
                (result->string (test-result-kind runner) #:colorize? color?)
                test-name test-case-name
                (and show-duration? time-elapsed-seconds)))

      (unless (and errors-only? (not (test-failed? runner)))
        (format #t "test-name: ~A~%" (result 'test-name))
        (format #t "location: ~A~%"
                (string-append (result 'source-file) ":"
                               (number->string (result 'source-line))))
        (test-display "source" (result 'source-form) #:pretty? #t)
        (when (result? 'expected-value)
          (test-display "expected-value" (result 'expected-value)))
        (when (result? 'expected-error)
          (test-display "expected-error" (result 'expected-error) #:pretty? #t))
        (when (result? 'actual-value)
          (test-display "actual-value" (result 'actual-value)))
        (when (result? 'actual-error)
          (test-display "actual-error" (result 'actual-error) #:pretty? #t))
        (format #t "result: ~a~%" (result->string (result 'result-kind)))
        (newline))

      (format trs-port ":test-result: ~A ~A [~,3fs]~%"
              (result->string (test-result-kind runner))
              (test-runner-test-name runner) time-elapsed-seconds)))

  (define (test-on-group-end-gnu runner)
    ;; Procedure called by a 'test-end', including at the end of a test-group.
    (let ((fail (or (positive? (test-runner-fail-count runner))
                    (positive? (test-runner-xpass-count runner))))
          (skip (or (positive? (test-runner-skip-count runner))
                    (positive? (test-runner-xfail-count runner)))))
      ;; XXX: The global results need some refinements for XPASS.
      (format trs-port ":global-test-result: ~A~%"
              (if fail "FAIL" (if skip "SKIP" "PASS")))
      (format trs-port ":recheck: ~A~%"
              (if fail "yes" "no"))
      (format trs-port ":copy-in-global-log: ~A~%"
              (if (or fail skip) "yes" "no"))
      (when brief?
        ;; Display the global test group result on the console.
        (format out-port "~A: ~A~%"
                (result->string (if fail 'fail (if skip 'skip 'pass))
                                #:colorize? color?)
                test-name))
      #f))

  (let ((runner (test-runner-null)))
    (test-runner-on-test-begin! runner test-on-test-begin-gnu)
    (test-runner-on-test-end! runner test-on-test-end-gnu)
    (test-runner-on-group-end! runner test-on-group-end-gnu)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))


;;;
;;; SRFI 64 test specifiers.
;;;
(define (test-match-name* regexp)
  "Return a test specifier that matches a test name against REGEXP."
  (lambda (runner)
    (string-match regexp (test-runner-test-name runner))))

(define (test-match-name*/negated regexp)
  "Return a negated test specifier version of test-match-name*."
  (lambda (runner)
    (not (string-match regexp (test-runner-test-name runner)))))

;;; XXX: test-match-all is a syntax, which isn't convenient to use with a list
;;; of test specifiers computed at run time.  Copy this SRFI 64 internal
;;; definition here, which is the procedural equivalent of 'test-match-all'.
(define (%test-match-all . pred-list)
  (lambda (runner)
    (let ((result #t))
      (let loop ((l pred-list))
	(if (null? l)
	    result
	    (begin
	      (if (not ((car l) runner))
		  (set! result #f))
	      (loop (cdr l))))))))


;;;
;;; Entry point.
;;;

(define (main . args)
  (let* ((opts   (getopt-long (command-line) %options))
         (option (cut option-ref opts <> <>)))
    (cond
     ((option 'help #f)    (show-help))
     ((option 'version #f) (format #t "test-driver.scm ~A~%" script-version))
     (else
      (let* ((log (and=> (option 'log-file #f) (cut open-file <> "w0")))
             (trs (and=> (option 'trs-file #f) (cut open-file <> "wl")))
             (out (duplicate-port (current-output-port) "wl"))
             (test-name (option 'test-name #f))
             (select (option 'select #f))
             (exclude (option 'exclude #f))
             (test-specifiers (filter-map
                               identity
                               (list (and=> select test-match-name*)
                                     (and=> exclude test-match-name*/negated))))
             (test-specifier (apply %test-match-all test-specifiers))
             (color-tests (if (assoc 'color-tests opts)
                              (option->boolean opts 'color-tests)
                              #t)))
        (when log
          (redirect-port log (current-output-port))
          (redirect-port log (current-warning-port))
          (redirect-port log (current-error-port)))
        (test-with-runner
            (test-runner-gnu test-name
                             #:color? color-tests
                             #:brief? (option->boolean opts 'brief)
                             #:errors-only? (option->boolean opts 'errors-only)
                             #:show-duration? (option->boolean
                                               opts 'show-duration)
                             #:out-port out #:trs-port trs)
          (test-apply test-specifier
                      (lambda _
                        (load-from-path test-name))))
        (and=> log close-port)
        (and=> trs close-port)
        (close-port out))))
    (exit 0)))

;;; Local Variables:
;;; eval: (add-hook 'write-file-functions 'time-stamp)
;;; time-stamp-start: "(define script-version \""
;;; time-stamp-format: "%:y-%02m-%02d.%02H"
;;; time-stamp-time-zone: "UTC"
;;; time-stamp-end: "\") ;UTC"
;;; End:

;;;; test-driver.scm ends here.
