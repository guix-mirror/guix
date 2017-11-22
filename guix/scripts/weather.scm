;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix scripts weather)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix progress)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix grafts)
  #:use-module ((guix build syscalls) #:select (terminal-columns))
  #:use-module (guix scripts substitute)
  #:use-module (gnu packages)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-weather))

(define (all-packages)
  "Return the list of public packages we are going to query."
  (fold-packages (lambda (package result)
                   (match (package-replacement package)
                     ((? package? replacement)
                      (cons* replacement package result))
                     (#f
                      (cons package result))))
                 '()))

(define (call-with-progress-reporter reporter proc)
  "This is a variant of 'call-with-progress-reporter' that works with monadic
scope."
  ;; TODO: Move to a more appropriate place.
  (with-monad %store-monad
    (start-progress-reporter! reporter)
    (mlet* %store-monad ((report -> (lambda ()
                                      (progress-reporter-report! reporter)))
                         (result (proc report)))
      (stop-progress-reporter! reporter)
      (return result))))

(define* (package-outputs packages
                          #:optional (system (%current-system)))
  "Return the list of outputs of all of PACKAGES for the given SYSTEM."
  (let ((packages (filter (cut supported-package? <> system) packages)))
    (format (current-error-port)
            (G_ "computing ~h package derivations for ~a...~%")
            (length packages) system)

    (call-with-progress-reporter (progress-reporter/bar (length packages))
      (lambda (report)
        (foldm %store-monad
               (lambda (package result)
                 (mlet %store-monad ((drv (package->derivation package system
                                                               #:graft? #f)))
                   (report)
                   (match (derivation->output-paths drv)
                     (((names . items) ...)
                      (return (append items result))))))
               '()
               packages)))))

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

(define-syntax-rule (let/time ((time result exp)) body ...)
  (call-with-time (lambda () exp) (lambda (time result) body ...)))

(define (report-server-coverage server items)
  "Report the subset of ITEMS available as substitutes on SERVER."
  (define MiB (* (expt 2 20) 1.))

  (format #t (G_ "looking for ~h store items on ~a...~%")
          (length items) server)

  (let/time ((time narinfos (lookup-narinfos server items)))
    (format #t "~a~%" server)
    (let ((obtained  (length narinfos))
          (requested (length items))
          (sizes     (filter-map narinfo-file-size narinfos))
          (time      (+ (time-second time)
                        (/ (time-nanosecond time) 1e9))))
      (format #t (G_ "  ~2,1f% substitutes available (~h out of ~h)~%")
              (* 100. (/ obtained requested 1.))
              obtained requested)
      (let ((total (/ (reduce + 0 sizes) MiB)))
        (match (length sizes)
          ((? zero?)
           (format #t (G_  "  unknown substitute sizes~%")))
          (len
           (if (= len obtained)
               (format #t (G_ "  ~,1h MiB of nars (compressed)~%") total)
               (format #t (G_ "  at least ~,1h MiB of nars (compressed)~%")
                       total)))))
      (format #t (G_ "  ~,1h MiB on disk (uncompressed)~%")
              (/ (reduce + 0 (map narinfo-size narinfos)) MiB))
      (format #t (G_ "  ~,3h seconds per request (~,1h seconds in total)~%")
              (/ time requested 1.) time)
      (format #t (G_ "  ~,1h requests per second~%")
              (/ requested time 1.)))))


;;;
;;; Command-line options.
;;;

(define (show-help)
  (display (G_ "Usage: guix weather [OPTIONS]
Report the availability of substitutes.\n"))
  (display (G_ "
      --substitute-urls=URLS
                         check for available substitutes at URLS"))
  (display (G_ "
  -m, --manifest=MANIFEST
                         look up substitutes for packages specified in MANIFEST"))
  (display (G_ "
  -s, --system=SYSTEM    consider substitutes for SYSTEM--e.g., \"i686-linux\""))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  (list  (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix challenge")))

         (option '("substitute-urls") #t #f
                 (lambda (opt name arg result . rest)
                   (let ((urls (string-tokenize arg)))
                     (for-each (lambda (url)
                                 (unless (string->uri url)
                                   (leave (G_ "~a: invalid URL~%") url)))
                               urls)
                     (apply values
                            (alist-cons 'substitute-urls urls
                                        (alist-delete 'substitute-urls result))
                            rest))))
         (option '(#\m "manifest") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'manifest arg result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg result)))))

(define %default-options
  `((substitute-urls . ,%default-substitute-urls)))

(define (load-manifest file)
  "Load the manifest from FILE and return the list of packages it refers to."
  (let* ((user-module (make-user-module '((guix profiles) (gnu))))
         (manifest    (load* file user-module)))
    (map manifest-entry-item
         (manifest-transitive-entries manifest))))


;;;
;;; Entry point.
;;;

(define (guix-weather . args)
  (with-error-handling
    (parameterize ((current-terminal-columns (terminal-columns)))
      (let* ((opts     (parse-command-line args %options
                                           (list %default-options)
                                           #:build-options? #f))
             (urls     (assoc-ref opts 'substitute-urls))
             (systems  (match (filter-map (match-lambda
                                            (('system . system) system)
                                            (_ #f))
                                          opts)
                         (() (list (%current-system)))
                         (systems systems)))
             (packages (let ((file (assoc-ref opts 'manifest)))
                         (if file
                             (load-manifest file)
                             (all-packages))))
             (items    (with-store store
                         (parameterize ((%graft? #f))
                           (concatenate
                            (run-with-store store
                              (mapm %store-monad
                                    (lambda (system)
                                      (package-outputs packages system))
                                    systems)))))))
        (for-each (lambda (server)
                    (report-server-coverage server items))
                  urls)))))

;;; Local Variables:
;;; eval: (put 'let/time 'scheme-indent-function 1)
;;; End:
