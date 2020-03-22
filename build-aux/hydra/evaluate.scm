;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

;;; This program replicates the behavior of Hydra's 'hydra-eval-guile-job'.
;;; It evaluates the Hydra job defined by the program passed as its first
;;; arguments and outputs an sexp of the jobs on standard output.

(use-modules (guix store)
             (guix git-download)
             ((guix build utils) #:select (with-directory-excursion))
             ((guix ui) #:select (build-notifier))
             (srfi srfi-19)
             (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 format))

(define %top-srcdir
  (and=> (assq-ref (current-source-location) 'filename)
         (lambda (file)
           (canonicalize-path
            (string-append (dirname file) "/../..")))))

(define %user-module
  ;; Hydra user module.
  (let ((m (make-module)))
    (beautify-user-module! m)
    m))

(cond-expand
  (guile-2.2
   ;; Guile 2.2.2 has a bug whereby 'time-monotonic' objects have seconds and
   ;; nanoseconds swapped (fixed in Guile commit 886ac3e).  Work around it.
   (define time-monotonic time-tai))
  (else #t))

(define (call-with-time thunk kont)
  "Call THUNK and pass KONT the elapsed time followed by THUNK's return
values."
  (let* ((start  (current-time time-monotonic))
         (result (call-with-values thunk list))
         (end    (current-time time-monotonic)))
    (apply kont (time-difference end start) result)))

(define (call-with-time-display thunk)
  "Call THUNK and write to the current output port its duration."
  (call-with-time thunk
    (lambda (time . results)
      (format #t "~,3f seconds~%"
              (+ (time-second time)
                 (/ (time-nanosecond time) 1e9)))
      (apply values results))))

(define (assert-valid-job job thing)
  "Raise an error if THING is not an alist with a valid 'derivation' entry.
Otherwise return THING."
  (unless (and (list? thing)
               (and=> (assoc-ref thing 'derivation)
                      (lambda (value)
                        (and (string? value)
                             (string-suffix? ".drv" value)))))
    (error "job did not produce a valid alist" job thing))
  thing)


;; Without further ado...
(match (command-line)
  ((command file cuirass? ...)
   ;; Load FILE, a Scheme file that defines Hydra jobs.
   (let ((port (current-output-port))
         (real-build-things build-things))
     (with-store store
       ;; Make sure we don't resort to substitutes.
       (set-build-options store
                          #:use-substitutes? #f
                          #:substitute-urls '())

       ;; The evaluation of Guix itself requires building a "trampoline"
       ;; program, and possibly everything it depends on.  Thus, allow builds
       ;; but print a notification.
       (with-build-handler (build-notifier #:use-substitutes? #f)

         ;; Add %TOP-SRCDIR to the store with a proper Git predicate so we work
         ;; from a clean checkout
         (let ((source (add-to-store store "guix-source" #t
                                     "sha256" %top-srcdir
                                     #:select? (git-predicate %top-srcdir))))
           (with-directory-excursion source
             (save-module-excursion
              (lambda ()
                (set-current-module %user-module)
                (format (current-error-port)
                        "loading '~a' relative to '~a'...~%"
                        file source)
                (primitive-load file))))

           ;; Call the entry point of FILE and print the resulting job sexp.
           (pretty-print
            (match ((module-ref %user-module
                                (if (equal? cuirass? "cuirass")
                                    'cuirass-jobs
                                    'hydra-jobs))
                    store `((guix
                             . ((file-name . ,source)))))
              (((names . thunks) ...)
               (map (lambda (job thunk)
                      (format (current-error-port) "evaluating '~a'... " job)
                      (force-output (current-error-port))
                      (cons job
                            (assert-valid-job job
                                              (call-with-time-display thunk))))
                    names thunks)))
            port))))))
  ((command _ ...)
   (format (current-error-port) "Usage: ~a FILE [cuirass]
Evaluate the Hydra or Cuirass jobs defined in FILE.~%"
           command)
   (exit 1)))

;;; Local Variables:
;;; eval: (put 'call-with-time 'scheme-indent-function 1)
;;; End:

