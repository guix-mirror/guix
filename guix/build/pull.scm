;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build pull)
  #:use-module (guix build utils)
  #:use-module (system base compile)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (build-guix))

;;; Commentary:
;;;
;;; Helpers for the 'guix pull' command to unpack and build Guix.
;;;
;;; Code:

(define (call-with-process thunk)
  "Run THUNK in a separate process that will return 0 if THUNK terminates
normally, and 1 if an exception is raised."
  (match (primitive-fork)
    (0
     (catch #t
       (lambda ()
         (thunk)
         (primitive-exit 0))
       (lambda (key . args)
         (print-exception (current-error-port) #f key args)
         (primitive-exit 1))))
    (pid
     #t)))

(define* (report-build-progress total completed cont
                                #:optional (log-port (current-error-port)))
  "Report that COMPLETED out of TOTAL files have been completed, and call
CONT."
  (display #\cr log-port)
  (format log-port "compiling...\t~5,1f% of ~d files" ;FIXME: i18n
          (* 100. (/ completed total)) total)
  (force-output log-port)
  (cont))

(define* (p-for-each proc lst
                     #:optional (max-processes (current-processor-count))
                     #:key (progress report-build-progress))
  "Invoke PROC for each element of LST in a separate process, using up to
MAX-PROCESSES processes in parallel.  Call PROGRESS at each step, passing it
the continuation.  Raise an error if one of the processes exit with non-zero."
  (define total
    (length lst))

  (define (wait-for-one-process)
    (match (waitpid WAIT_ANY)
      ((_ . status)
       (unless (zero? (status:exit-val status))
         (error "process failed" proc status)))))

  (let loop ((lst       lst)
             (running   0)
             (completed 0))
    (match lst
      (()
       (or (zero? running)
           (let ((running   (- running 1))
                 (completed (+ completed 1)))
             (wait-for-one-process)
             (progress total completed
                       (lambda ()
                         (loop lst running completed))))))
      ((head . tail)
       (if (< running max-processes)
           (let ((running (+ 1 running)))
             (call-with-process (cut proc head))
             (progress total completed
                       (lambda ()
                         (loop tail running completed))))
           (let ((running   (- running 1))
                 (completed (+ completed 1)))
             (wait-for-one-process)
             (progress total completed
                       (lambda ()
                         (loop lst running completed)))))))))

(define* (build-guix out tarball
                     #:key tar gzip gcrypt
                     (debug-port (%make-void-port "w")))
  "Build and install Guix in directory OUT using source from TARBALL.  Write
any debugging output to DEBUG-PORT."
  (setvbuf (current-output-port) _IOLBF)
  (setvbuf (current-error-port) _IOLBF)

  (setenv "PATH" (string-append tar "/bin:" gzip "/bin"))

  (format debug-port "extracting '~a'...~%" tarball)
  (system* "tar" "xf" tarball)

  (match (scandir "." (lambda (name)
                        (and (not (member name '("." "..")))
                             (file-is-directory? name))))
    ((dir)
     (chdir dir))
    (x
     (error "tarball did not produce a single source directory" x)))

  (format #t "copying and compiling to '~a'...~%" out)

  ;; Copy everything under guix/ and gnu/ plus {guix,gnu}.scm.
  (copy-recursively "guix" (string-append out "/guix")
                    #:log debug-port)
  (copy-recursively "gnu" (string-append out "/gnu")
                    #:log debug-port)
  (copy-file "guix.scm" (string-append out "/guix.scm"))
  (copy-file "gnu.scm" (string-append out "/gnu.scm"))

  ;; Add a fake (guix config) module to allow the other modules to be
  ;; compiled.  The user's (guix config) is the one that will be used.
  (copy-file "guix/config.scm.in"
             (string-append out "/guix/config.scm"))
  (substitute* (string-append out "/guix/config.scm")
    (("@LIBGCRYPT@")
     (string-append gcrypt "/lib/libgcrypt")))

  ;; Augment the search path so Scheme code can be compiled.
  (set! %load-path (cons out %load-path))
  (set! %load-compiled-path (cons out %load-compiled-path))

  ;; Compile the .scm files.  Do that in independent processes, à la
  ;; 'make -j', to work around <http://bugs.gnu.org/15602> (FIXME).
  ;; This ensures correctness, but is overly conservative and slow.
  ;; The solution initially implemented (and described in the bug
  ;; above) was slightly faster but consumed memory proportional to the
  ;; number of modules, which quickly became unacceptable.
  (p-for-each (lambda (file)
                (let ((go (string-append (string-drop-right file 4)
                                         ".go")))
                  (format debug-port "~%compiling '~a'...~%" file)
                  (parameterize ((current-warning-port debug-port))
                    (compile-file file
                                  #:output-file go
                                  #:opts
                                  %auto-compilation-options))))

              (filter (cut string-suffix? ".scm" <>)

                      ;; Build guix/*.scm before gnu/*.scm to speed
                      ;; things up.
                      (sort (find-files out "\\.scm")
                            (let ((guix (string-append out "/guix"))
                                  (gnu  (string-append out "/gnu")))
                              (lambda (a b)
                                (or (and (string-prefix? guix a)
                                         (string-prefix? gnu b))
                                    (string<? a b)))))))

  ;; Remove the "fake" (guix config).
  (delete-file (string-append out "/guix/config.scm"))
  (delete-file (string-append out "/guix/config.go"))

  (newline)
  #t)

;;; pull.scm ends here
